# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Copyright 2006-2008 Floréal Morandat <morandat@lirmm.fr>
# Copyright 2008 Jean Privat <jean@pryen.org>
# Copyright 2009 Jean-Sebastien Gelinas <calestar@gmail.com>
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# This module is used to load a metamodel
package mmloader

import metamodel
import opts
import location

class Message
	super Comparable
	redef type OTHER: Message

	readable var _location: nullable Location
	readable var _text: String

	redef fun <(other: OTHER): Bool do
		if location == null then return true
		if other.location == null then return false

		return location.as(not null) < other.location.as(not null)
	end

	redef fun to_s: String do
		var l = location
		if l == null then
			return text
		else
			return "{l}: {text}"
		end
	end
end

# Global context for tools
class ToolContext
	super MMContext
	# Number of errors
	readable var _error_count: Int = 0

	# Number of warnings
	readable var _warning_count: Int = 0

	# Directory where to generate log files
	readable var _log_directory: String = "logs"

	# Messages
	var _messages: Array[Message] = new Array[Message]
	var _message_sorter: ComparableSorter[Message] = new ComparableSorter[Message]

	fun check_errors
	do
		if _messages.length > 0 then
			_message_sorter.sort(_messages)

			for m in _messages do
				stderr.write("{m}\n")
			end

			_messages.clear
		end

		if error_count > 0 then exit(1)
	end

	# Display an error
	fun error(l: nullable Location, s: String)
	do
		_messages.add(new Message(l,s))
		_error_count = _error_count + 1
		if opt_stop_on_first_error.value then check_errors
	end

	# Add an error, show errors and quit
	fun fatal_error(l: nullable Location, s: String)
	do
		error(l,s)
		check_errors
	end

	# Display a warning
	fun warning(l: nullable Location, s: String)
	do
		if _opt_warn.value == 0 then return
		_messages.add(new Message(l,s))
		if _opt_warn.value == 1 then
			_warning_count = _warning_count + 1
		else
			_error_count = _error_count + 1
		end
		if opt_stop_on_first_error.value then check_errors
	end

	# Display an info
	fun info(s: String, level: Int)
	do
		if level <= verbose_level then
			print "{s}"
		end
	end

	# Paths where to locate modules files
	readable var _paths: Array[String] = new Array[String]

	# Root source directory
	readable var _root_source_directory: nullable MMDirectory = null
	
	# Root directory for other sources (-I, enviroment variables, etc)
	readable var _additional_root_source_directories: List[MMDirectory] = new List[MMDirectory]

	# List of module loaders
	var _loaders: Array[ModuleLoader] = new Array[ModuleLoader]
	
	# Global OptionContext
	readable var _option_context: OptionContext = new OptionContext

	# Option --warn
	readable var _opt_warn: OptionCount = new OptionCount("Show warnings", "-W", "--warn")

	# Option --path
	readable var _opt_path: OptionArray = new OptionArray("Set include path for loaders (may be used more than once)", "-I", "--path")

	# Option --log
	readable var _opt_log: OptionBool = new OptionBool("Generate various log files", "--log")

	# Option --log-dir
	readable var _opt_log_dir: OptionString = new OptionString("Directory where to generate log files", "--log-dir")

	# Option --only-metamodel
	readable var _opt_only_metamodel: OptionBool = new OptionBool("Stop after meta-model processing", "--only-metamodel")

	# Option --only-parse
	readable var _opt_only_parse: OptionBool = new OptionBool("Only proceed to parse step of loaders", "--only-parse")

	# Option --help
	readable var _opt_help: OptionBool = new OptionBool("Show Help (This screen)", "-h", "-?", "--help")

	# Option --version
	readable var _opt_version: OptionBool = new OptionBool("Show version and exit", "--version")

	# Option --verbose
	readable var _opt_verbose: OptionCount = new OptionCount("Verbose", "-v", "--verbose")

	# Option --stop-on-first-error
	readable var _opt_stop_on_first_error: OptionBool = new OptionBool("Stop on first error", "--stop-on-first-error")

	# Verbose level
	readable var _verbose_level: Int = 0

	init
	do
		super
		option_context.add_option(opt_warn, opt_stop_on_first_error, opt_path, opt_log, opt_log_dir, opt_only_parse, opt_only_metamodel, opt_help, opt_version, opt_verbose)
	end
	
	# Creates a MMDirectory for each root directories specified in paths
	fun prepare_module_fetching
	do
		for p in paths do
			var dir = new MMDirectory(p.to_symbol, p, null)
			_additional_root_source_directories.push(dir)
		end
	end

	# Parse and process the options given on the command line
	fun process_options
	do
		# init options
		option_context.parse(args)

		# Set verbose level
		_verbose_level = opt_verbose.value

		# Setup the paths value
		paths.append(opt_path.value)

		var path_env = once ("NIT_PATH".to_symbol).environ
		if not path_env.is_empty then 
			paths.append(path_env.split_with(':'))
		end

		path_env = once ("NIT_DIR".to_symbol).environ
		if not path_env.is_empty then 
			var libname = "{path_env}/lib"
			if libname.file_exists then paths.add(libname)
		end

		var libname = "{sys.program_name.dirname}/../lib"
		if libname.file_exists then paths.add(libname)

		if opt_log_dir.value != null then _log_directory = opt_log_dir.value.as(not null)
		if _opt_log.value then
			# Make sure the output directory exists
			log_directory.mkdir
		end
	end

	# Load and process a module in a directory (or a parent directory).
	# If the module is already loaded, just return it without further processing.
	# If no module is found, just return null without complaining.
	# Search locally
	private fun try_to_load(module_name: MMModuleName, dir: MMDirectory): nullable MMModule
	do
		print "in  try_to_load module_name {module_name} dir {dir}"

		var mod : nullable MMModule
		
		# get module if loaded
		mod = dir.get_module_if_loaded(module_name)
		if mod != null then return mod
		# check if name is a module folder
#		var module_folder_name = module_name.get_as_module_foler
#		mod = dir.get_module_if_loaded(module_folder_name)
#		if mod != null then return mod
		
		
		var module_found_already = false
		var mod2 : nullable MMModule
		mod2 = null.as(nullable MMModule)
		var full_name = "".to_symbol
		
		for l in _loaders do
			
			var is_module_handled_with_usual_name = false
			var dir2 = l.try_to_load_dir_with_full_path(module_name, dir)
			
			if dir2 != null then
				if l.can_handle(module_name.name, dir2) then
					# file with usual name exists
					is_module_handled_with_usual_name = true
					
					full_name = dir2.full_name_for(module_name.name)
					if module_found_already == true then
						fatal_error(null, "Error: module {full_name} can be loaded from many loaders")
						abort
					end
					if _processing_modules.has(full_name) then
						# FIXME: Generate better error
						fatal_error(null, "Error: Dependency loop for module {full_name}")
					end
					_processing_modules.add(full_name)
					print "PROCESS {full_name}"
					mod2 = l.load_and_process_module(self, module_name.name, dir2)
					print "-PROCESS {full_name}"
					_processing_modules.remove(full_name)
					
					dir2.add_module(mod2)
					module_found_already = true
				end
			end
			
			# try to interpret a mm_module_name.name as a folder 
			# containing a nit module with same name
			
#			var new_module_name = module_name.get_as_module_foler
			
#			var dir3 = l.try_to_load_dir_with_full_path(new_module_name, dir)
			
#			if dir3 != null then
#				if is_module_handled_with_usual_name then
#					fatal_error(null, "Error: module {full_name} exists as a module unit and a module folder.")
#					abort
#				end

#				if l.can_handle(module_name.name, dir3) then

#					full_name = dir3.full_name_for(module_name.name)
#					if module_found_already == true then
#						fatal_error(null, "Error: module {full_name} can be loaded from many loaders")
#						abort
#					end
#					if _processing_modules.has(full_name) then
#						# FIXME: Generate better error
#						fatal_error(null, "Error: Dependency loop for module {full_name}")
#					end
#					_processing_modules.add(full_name)
#					print "PROCESS {full_name}"
#					mod2 = l.load_and_process_module(self, module_name.name, dir3)
#					print "-PROCESS {full_name}"
#					_processing_modules.remove(full_name)
					
#					dir3.add_module(mod2)
#					module_found_already = true
#				end
#			end
		end
		
		print "out try_to_load module_name {module_name} dir {dir}"
		return mod2 
	end

	# List of module currently processed.
	# Used to prevent dependence loops.
	var _processing_modules: HashSet[Symbol] = new HashSet[Symbol]

	# Locate, load and analysis a module (and its supermodules) from its file name.
	# If the module is already loaded, just return it without further processing.
	# Beware, the files are automatically considered root of their directory.
	fun get_module_from_filename(filename: String): MMModule
	do
		print "in  get_module_from_filename filename {filename}"
		
		var path = filename.dirname
		var module_name = filename.basename(".nit").to_symbol

		var mm_module_name = new MMModuleName(true, new List[Symbol], module_name)

		var dir = directory_for(path)
		
		_root_source_directory = dir
		
		# look for it in the path directory entered
		var m = try_to_load(mm_module_name, dir)
		

		print "out get_module_from_filename filename {filename}"

		if m != null then return m

		fatal_error(null, "Error: {filename} is not a NIT source module.")
		abort
	end

	# Locate, load and analysis a module (and its supermodules).
	# If the module is already loaded, just return it without further processing.
	fun get_module(module_name: MMModuleName, from: nullable MMModule): MMModule
	do
		if from == null then print "in  get_module module_name {module_name} from null"
		if from != null then print "in  get_module module_name {module_name} from {from}"

		var module_found_in_paths = new List[Couple[MMDirectory, String]]
		var mod_local: nullable MMModule = null
		var mod_in_other_dir: nullable MMModule = null
		var mod: nullable MMModule = null
		var root_dir: nullable MMDirectory = null
		var searched_dir = new HashSet[String]
		
		var module_name_as_folder = module_name.get_as_module_foler
		
		# or is it from other directories
		for dir in _additional_root_source_directories do
			if not searched_dir.has("{dir.path}{module_name.to_folder_path}") then
				mod_in_other_dir = try_to_load(module_name, dir)
				searched_dir.add("{dir.path}{module_name.to_folder_path}")
				if mod_in_other_dir != null then
					module_found_in_paths.push(new Couple[MMDirectory, String](dir, "Additional"))
					mod = mod_in_other_dir
				end
			end
		end
		
		# Is the module a folder?
		for dir in _additional_root_source_directories do
			if not searched_dir.has("{dir.path}{module_name_as_folder.to_folder_path}") then
				mod_in_other_dir = try_to_load(module_name_as_folder, dir)
				searched_dir.add("{dir.path}{module_name_as_folder.to_folder_path}")
				if mod_in_other_dir != null then
					module_found_in_paths.push(new Couple[MMDirectory, String](dir, "Additional-module-folder"))
					mod = mod_in_other_dir
				end
			end
		end
		
		if from != null then root_dir = from.directory.get_root_dir
		
		# not from root but has a dir -> search local up to roots
		# is_from_root -> search from roots only
		# has no from dir -> search from roots only
		
		if not module_name.is_from_root and from != null then
			# Look from local and local parents

			var search_dir = from.directory.as(nullable MMDirectory)
			
			while search_dir != null 
			do
				if not searched_dir.has("{search_dir.path}{module_name.to_folder_path}") then
					mod_local = try_to_load(module_name, search_dir)
					searched_dir.add("{search_dir.path}{module_name.to_folder_path}")
					if mod_local != null then
						module_found_in_paths.push(new Couple[MMDirectory, String](search_dir, "Local"))
						mod = mod_local
					end
				end 
				
				# Is the module name a folder?
				if not searched_dir.has("{search_dir.path}{module_name_as_folder.to_folder_path}") then
					mod_local = try_to_load(module_name_as_folder, search_dir)
					searched_dir.add("{search_dir.path}{module_name_as_folder.to_folder_path}")
					if mod_local != null then
						module_found_in_paths.push(new Couple[MMDirectory, String](search_dir, "Local-module-folder"))
						mod = mod_local
					end
				end 
				search_dir = search_dir.parent
			end
		end
			
		
		# maybe it's from main root source
#		if not searched_dir.has(_root_source_directory.as(not null)) then
#			var mod_from_root = try_to_load(module_name, _root_source_directory.as(not null))
#			
#			searched_dir.add(_root_source_directory.as(not null))
#			
#			if mod_from_root != null then
#				module_found_in_paths.push(new Couple[MMDirectory, String](_root_source_directory.as(not null), "Root"))
#				mod = mod_from_root
#			end
#		end
		
		
		
		# Check for duplicate
		
		if module_found_in_paths.length > 1 then
			# FIXME: Generate better error
			var out = ""
			
			for couple in module_found_in_paths
			do
				out += "\n\t" + couple.first.path + "->" + couple.second #.as(not null)
			end
			
			if from != null then fatal_error(null, "Error: Ambiguous module import '{module_name}' from {from.full_name}. Found in {out}")
			if from == null then fatal_error(null, "Error: Ambiguous module import '{module_name}' from null. Found in {out}")
			abort
		else if module_found_in_paths.length == 0 then
			# FIXME: Generate better error
			if from != null then fatal_error(null, "Error: No ressource found for module '{module_name}' imported from '{from.location}'.")
			if from == null then fatal_error(null, "Error: No ressource found for module '{module_name}'")
			abort
		end
		
#		if from == null then print "out get_module module_name {module_name} from null"
#		if from != null then print "out get_module module_name {module_name} from {from}"
		return mod.as(not null)
	end

	# Return the module directory associated with a given path
	private fun directory_for(path: String): MMDirectory
	do
		if _path_dirs.has_key(path) then return _path_dirs[path]
		var dir = new MMDirectory(path.to_symbol, path, null) 
		_path_dirs[path] = dir
		return dir
	end

	# Association bwtween plain path and module directories
	var _path_dirs: Map[String, MMDirectory] = new HashMap[String, MMDirectory]

	# Register a new module loader
	fun register_loader(ml: ModuleLoader) do _loaders.add(ml)
end

# A load handler know how to load a specific module type
class ModuleLoader
	# Type of module loaded by the loader
	type MODULE: MMModule

	# Extension that the loadhandler accepts
	fun file_type: String is abstract

	# Try to load a new module directory
	fun try_to_load_dir(dirname: Symbol, parent_dir: MMDirectory): nullable MMDirectory
	do
#		print "in  try_to_load_dir dirname {dirname} parent_dir {parent_dir}"
		var fname = "{parent_dir.path}{dirname}/"
		if not fname.file_exists then 
#			print "loc try_to_load_dir fname.file_exists == false"
#			print "out try_to_load_dir dirname {dirname} parent_dir {parent_dir}"
#			print "loc try_to_load_dir fname {fname}: file exists : no"
			return null
		end

#		print "loc try_to_load_dir fname {fname}: file exists : yes"
		var dir = new MMDirectory(parent_dir.full_name_for(dirname), fname, parent_dir)

#		print "out try_to_load_dir dirname {dirname} parent_dir {parent_dir}"
		return dir
	end

	# Try to load a new module directory full path
	fun try_to_load_dir_with_full_path(module_name: MMModuleName, parent_dir: MMDirectory): nullable MMDirectory
	do
#		print "in  try_to_load_dir_with_full_path module_name {module_name} parent_dir {parent_dir}"

		var fname = "{parent_dir.path}{module_name.to_folder_path}"
		
		if not fname.file_exists then 
			print "loc try_to_load_dir_with_full_path fname {fname}: file exists : no"
#			print "loc try_to_load_dir_with_full_path fname.file_exist == false"
#			print "out try_to_load_dir_with_full_path module_name {module_name} parent_dir {parent_dir}"
			return null
		end

		print "loc try_to_load_dir_with_full_path fname {fname}: file exists : yes"
#		print "out try_to_load_dir_with_full_path module_name {module_name} parent_dir {parent_dir}"
		return parent_dir.make_path_and_get_last_dir(module_name.path)
	end

	# Can the loadhandler load a given module?
	# Return the file found
	fun can_handle(module_name: Symbol, dir: MMDirectory): Bool
	do
#		print "in  can_handle module_name {module_name} dir {dir}"

		# dir.path is assumed to be ended by a '/' character
		var fname = "{dir.path}{module_name}.{file_type}"
#		print "loc can_handle fname {fname}"

		if fname.file_exists then
#			print "out can_handle module_name {module_name} dir {dir}: yes"
			print "loc can_handle fname {fname}: yes"
			return true
		end

		print "loc can_handle fname {fname}: no"
#		print "out can_handle module_name {module_name} dir {dir}: no"
		return false
	end

	# Load the module and process it
	# filename is the result of can_handle
	fun load_and_process_module(context: ToolContext, module_name: Symbol, dir: MMDirectory): MODULE
	do
#		print "in  load_and_process_module module_name {module_name} dir {dir}"

		# dir.path is assumed to be ended by a '/' character
		var filename = "{dir.path}{module_name}.{file_type}"
		print "loc load_and_process_module filename {filename}"
		
		var m = load_module(context, module_name, dir, filename)
		if not context.opt_only_parse.value then process_metamodel(context, m)

		print "out load_and_process_module module_name {module_name} dir {dir}"
		return m
	end

	# Load an parse the module
	private fun load_module(context: ToolContext, module_name: Symbol, dir: MMDirectory, filename: String): MODULE
	do
		print "in  load_module module_name {module_name} dir {dir} filename {filename}"
	
		var file: IFStream
		if filename == "-" then
			file = stdin
		else
			file = new IFStream.open(filename.to_s)
		end

		if file.eof then
			context.fatal_error(null, "Error: Problem in opening file {filename}")
		end
		var m = parse_file(context, file, filename, module_name, dir)
		if file != stdin then file.close
		print "out load_module module_name {module_name} dir {dir} filename {filename}"
		return m
	end

	# Parse the file to load a module
	protected fun parse_file(context: ToolContext, file: IFStream, filename: String, module_name: Symbol, dir: MMDirectory): MODULE is abstract

	# Process a parsed module
	protected fun process_metamodel(context: ToolContext, mod: MODULE) is abstract
end
