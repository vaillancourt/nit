# This file is part of NIT ( http://www.nitlanguage.org ).
#
# Copyright 2008 Jean Privat <jean@pryen.org>
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

# Program used to test the NIT parser
package test_parser

import parser

class PrintTreeVisitor
	super Visitor
	var _rank: Int
	redef fun visit(n: nullable ANode)
	do
		if n == null then return
		printn("  " * _rank, n.to_s, " ... ", n.location, "\n")
		_rank = _rank + 1
		n.visit_all(self)
		_rank = _rank - 1
	end

	init
	do
		_rank = 0
	end
end

var no_print = false
var only_lexer = false
var need_help = false

while not args.is_empty and args.first.first == '-' do
	if args.first == "-n" then
		no_print = true
	else if args.first == "-l" then
		only_lexer = true
	else if args.first == "-p" then
		only_lexer = false 
	else if args.first == "-h" or args.first == "-?" then
		need_help = true
	else
		stderr.write("Unknown option {args.first}.\n")
		exit(0)
	end
	args.shift
end

if args.is_empty or need_help then
	print("usage:")
	print("  test_parser [options]... <filename.nit>...")
	print("options:")
	print("  -n	do not print anything")
	print("  -l	only lexer")
	print("  -p	lexer and parser (default)")
	print("  -h	print this help")
else
	for a in args do
		var f = new IFStream.open(a)
		var lexer = new Lexer(new SourceFile(a, f))
		if only_lexer then
			var token = lexer.next
			while not token isa EOF do
				if not no_print then
					print("Read token at {token.location} text='{token.text}'")
				end
				token = lexer.next
			end
			f.close
		else
			var parser = new Parser(lexer)
			var tree = parser.parse
			f.close

			if not no_print then
				(new PrintTreeVisitor).enter_visit(tree)
			end
		end
	end
end
