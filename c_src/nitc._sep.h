/* This C header file is generated by NIT to compile modules and programs that requires nitc. */
#ifndef nitc_sep
#define nitc_sep
#include "abstracttool._sep.h"
#include "compiling._sep.h"
#include <nit_common.h>

extern const classtable_elt_t VFT_NitCompiler[];
extern const char *LOCATE_nitc;
extern const int SFT_nitc[];
#define ID_NitCompiler (SFT_nitc[0])
#define COLOR_NitCompiler (SFT_nitc[1])
#define ATTR_nitc___NitCompiler____opt_output(recv) ATTR(recv, (SFT_nitc[2] + 0))
#define ATTR_nitc___NitCompiler____opt_boost(recv) ATTR(recv, (SFT_nitc[2] + 1))
#define ATTR_nitc___NitCompiler____opt_no_cc(recv) ATTR(recv, (SFT_nitc[2] + 2))
#define ATTR_nitc___NitCompiler____opt_cc_no_link(recv) ATTR(recv, (SFT_nitc[2] + 3))
#define ATTR_nitc___NitCompiler____opt_cc_lib_paths(recv) ATTR(recv, (SFT_nitc[2] + 4))
#define ATTR_nitc___NitCompiler____opt_cc_libs(recv) ATTR(recv, (SFT_nitc[2] + 5))
#define ATTR_nitc___NitCompiler____opt_cc_include_paths(recv) ATTR(recv, (SFT_nitc[2] + 6))
#define ATTR_nitc___NitCompiler____opt_global(recv) ATTR(recv, (SFT_nitc[2] + 7))
#define ATTR_nitc___NitCompiler____opt_global_no_STF_opt(recv) ATTR(recv, (SFT_nitc[2] + 8))
#define ATTR_nitc___NitCompiler____opt_global_no_DMR_opt(recv) ATTR(recv, (SFT_nitc[2] + 9))
#define ATTR_nitc___NitCompiler____opt_global_no_inline_get_set(recv) ATTR(recv, (SFT_nitc[2] + 10))
#define ATTR_nitc___NitCompiler____opt_global_no_out_of_init_get_test_opt(recv) ATTR(recv, (SFT_nitc[2] + 11))
#define ATTR_nitc___NitCompiler____opt_global_no_RFIMA(recv) ATTR(recv, (SFT_nitc[2] + 12))
#define ATTR_nitc___NitCompiler____opt_global_callgraph(recv) ATTR(recv, (SFT_nitc[2] + 13))
#define ATTR_nitc___NitCompiler____opt_clibdir(recv) ATTR(recv, (SFT_nitc[2] + 14))
#define ATTR_nitc___NitCompiler____opt_bindir(recv) ATTR(recv, (SFT_nitc[2] + 15))
#define ATTR_nitc___NitCompiler____opt_compdir(recv) ATTR(recv, (SFT_nitc[2] + 16))
#define ATTR_nitc___NitCompiler____opt_extension_prefix(recv) ATTR(recv, (SFT_nitc[2] + 17))
#define ATTR_nitc___NitCompiler____opt_output_format(recv) ATTR(recv, (SFT_nitc[2] + 18))
#define INIT_TABLE_POS_NitCompiler (SFT_nitc[3] + 0)
#define CALL_nitc___NitCompiler___opt_output(recv) ((nitc___NitCompiler___opt_output_t)CALL((recv), (SFT_nitc[3] + 1)))
#define CALL_nitc___NitCompiler___opt_boost(recv) ((nitc___NitCompiler___opt_boost_t)CALL((recv), (SFT_nitc[3] + 2)))
#define CALL_nitc___NitCompiler___opt_no_cc(recv) ((nitc___NitCompiler___opt_no_cc_t)CALL((recv), (SFT_nitc[3] + 3)))
#define CALL_nitc___NitCompiler___opt_cc_no_link(recv) ((nitc___NitCompiler___opt_cc_no_link_t)CALL((recv), (SFT_nitc[3] + 4)))
#define CALL_nitc___NitCompiler___opt_cc_lib_paths(recv) ((nitc___NitCompiler___opt_cc_lib_paths_t)CALL((recv), (SFT_nitc[3] + 5)))
#define CALL_nitc___NitCompiler___opt_cc_libs(recv) ((nitc___NitCompiler___opt_cc_libs_t)CALL((recv), (SFT_nitc[3] + 6)))
#define CALL_nitc___NitCompiler___opt_cc_include_paths(recv) ((nitc___NitCompiler___opt_cc_include_paths_t)CALL((recv), (SFT_nitc[3] + 7)))
#define CALL_nitc___NitCompiler___opt_global(recv) ((nitc___NitCompiler___opt_global_t)CALL((recv), (SFT_nitc[3] + 8)))
#define CALL_nitc___NitCompiler___opt_global_no_STF_opt(recv) ((nitc___NitCompiler___opt_global_no_STF_opt_t)CALL((recv), (SFT_nitc[3] + 9)))
#define CALL_nitc___NitCompiler___opt_global_no_DMR_opt(recv) ((nitc___NitCompiler___opt_global_no_DMR_opt_t)CALL((recv), (SFT_nitc[3] + 10)))
#define CALL_nitc___NitCompiler___opt_global_no_inline_get_set(recv) ((nitc___NitCompiler___opt_global_no_inline_get_set_t)CALL((recv), (SFT_nitc[3] + 11)))
#define CALL_nitc___NitCompiler___opt_global_no_out_of_init_get_test_opt(recv) ((nitc___NitCompiler___opt_global_no_out_of_init_get_test_opt_t)CALL((recv), (SFT_nitc[3] + 12)))
#define CALL_nitc___NitCompiler___opt_global_no_RFIMA(recv) ((nitc___NitCompiler___opt_global_no_RFIMA_t)CALL((recv), (SFT_nitc[3] + 13)))
#define CALL_nitc___NitCompiler___opt_global_callgraph(recv) ((nitc___NitCompiler___opt_global_callgraph_t)CALL((recv), (SFT_nitc[3] + 14)))
#define CALL_nitc___NitCompiler___opt_clibdir(recv) ((nitc___NitCompiler___opt_clibdir_t)CALL((recv), (SFT_nitc[3] + 15)))
#define CALL_nitc___NitCompiler___opt_bindir(recv) ((nitc___NitCompiler___opt_bindir_t)CALL((recv), (SFT_nitc[3] + 16)))
#define CALL_nitc___NitCompiler___opt_compdir(recv) ((nitc___NitCompiler___opt_compdir_t)CALL((recv), (SFT_nitc[3] + 17)))
#define CALL_nitc___NitCompiler___opt_extension_prefix(recv) ((nitc___NitCompiler___opt_extension_prefix_t)CALL((recv), (SFT_nitc[3] + 18)))
#define CALL_nitc___NitCompiler___opt_output_format(recv) ((nitc___NitCompiler___opt_output_format_t)CALL((recv), (SFT_nitc[3] + 19)))
#define CALL_nitc___NitCompiler___init(recv) ((nitc___NitCompiler___init_t)CALL((recv), (SFT_nitc[3] + 20)))
#define CALL_SUPER_nitc___NitCompiler___process_options(recv) ((nitc___NitCompiler___process_options_t)CALL((recv), (SFT_nitc[3] + 21)))
static const char * const LOCATE_nitc___NitCompiler___opt_output = "nitc::NitCompiler::opt_output";
val_t nitc___NitCompiler___opt_output(val_t p0);
typedef val_t (*nitc___NitCompiler___opt_output_t)(val_t p0);
static const char * const LOCATE_nitc___NitCompiler___opt_boost = "nitc::NitCompiler::opt_boost";
val_t nitc___NitCompiler___opt_boost(val_t p0);
typedef val_t (*nitc___NitCompiler___opt_boost_t)(val_t p0);
static const char * const LOCATE_nitc___NitCompiler___opt_no_cc = "nitc::NitCompiler::opt_no_cc";
val_t nitc___NitCompiler___opt_no_cc(val_t p0);
typedef val_t (*nitc___NitCompiler___opt_no_cc_t)(val_t p0);
static const char * const LOCATE_nitc___NitCompiler___opt_cc_no_link = "nitc::NitCompiler::opt_cc_no_link";
val_t nitc___NitCompiler___opt_cc_no_link(val_t p0);
typedef val_t (*nitc___NitCompiler___opt_cc_no_link_t)(val_t p0);
static const char * const LOCATE_nitc___NitCompiler___opt_cc_lib_paths = "nitc::NitCompiler::opt_cc_lib_paths";
val_t nitc___NitCompiler___opt_cc_lib_paths(val_t p0);
typedef val_t (*nitc___NitCompiler___opt_cc_lib_paths_t)(val_t p0);
static const char * const LOCATE_nitc___NitCompiler___opt_cc_libs = "nitc::NitCompiler::opt_cc_libs";
val_t nitc___NitCompiler___opt_cc_libs(val_t p0);
typedef val_t (*nitc___NitCompiler___opt_cc_libs_t)(val_t p0);
static const char * const LOCATE_nitc___NitCompiler___opt_cc_include_paths = "nitc::NitCompiler::opt_cc_include_paths";
val_t nitc___NitCompiler___opt_cc_include_paths(val_t p0);
typedef val_t (*nitc___NitCompiler___opt_cc_include_paths_t)(val_t p0);
static const char * const LOCATE_nitc___NitCompiler___opt_global = "nitc::NitCompiler::opt_global";
val_t nitc___NitCompiler___opt_global(val_t p0);
typedef val_t (*nitc___NitCompiler___opt_global_t)(val_t p0);
static const char * const LOCATE_nitc___NitCompiler___opt_global_no_STF_opt = "nitc::NitCompiler::opt_global_no_STF_opt";
val_t nitc___NitCompiler___opt_global_no_STF_opt(val_t p0);
typedef val_t (*nitc___NitCompiler___opt_global_no_STF_opt_t)(val_t p0);
static const char * const LOCATE_nitc___NitCompiler___opt_global_no_DMR_opt = "nitc::NitCompiler::opt_global_no_DMR_opt";
val_t nitc___NitCompiler___opt_global_no_DMR_opt(val_t p0);
typedef val_t (*nitc___NitCompiler___opt_global_no_DMR_opt_t)(val_t p0);
static const char * const LOCATE_nitc___NitCompiler___opt_global_no_inline_get_set = "nitc::NitCompiler::opt_global_no_inline_get_set";
val_t nitc___NitCompiler___opt_global_no_inline_get_set(val_t p0);
typedef val_t (*nitc___NitCompiler___opt_global_no_inline_get_set_t)(val_t p0);
static const char * const LOCATE_nitc___NitCompiler___opt_global_no_out_of_init_get_test_opt = "nitc::NitCompiler::opt_global_no_out_of_init_get_test_opt";
val_t nitc___NitCompiler___opt_global_no_out_of_init_get_test_opt(val_t p0);
typedef val_t (*nitc___NitCompiler___opt_global_no_out_of_init_get_test_opt_t)(val_t p0);
static const char * const LOCATE_nitc___NitCompiler___opt_global_no_RFIMA = "nitc::NitCompiler::opt_global_no_RFIMA";
val_t nitc___NitCompiler___opt_global_no_RFIMA(val_t p0);
typedef val_t (*nitc___NitCompiler___opt_global_no_RFIMA_t)(val_t p0);
static const char * const LOCATE_nitc___NitCompiler___opt_global_callgraph = "nitc::NitCompiler::opt_global_callgraph";
val_t nitc___NitCompiler___opt_global_callgraph(val_t p0);
typedef val_t (*nitc___NitCompiler___opt_global_callgraph_t)(val_t p0);
static const char * const LOCATE_nitc___NitCompiler___opt_clibdir = "nitc::NitCompiler::opt_clibdir";
val_t nitc___NitCompiler___opt_clibdir(val_t p0);
typedef val_t (*nitc___NitCompiler___opt_clibdir_t)(val_t p0);
static const char * const LOCATE_nitc___NitCompiler___opt_bindir = "nitc::NitCompiler::opt_bindir";
val_t nitc___NitCompiler___opt_bindir(val_t p0);
typedef val_t (*nitc___NitCompiler___opt_bindir_t)(val_t p0);
static const char * const LOCATE_nitc___NitCompiler___opt_compdir = "nitc::NitCompiler::opt_compdir";
val_t nitc___NitCompiler___opt_compdir(val_t p0);
typedef val_t (*nitc___NitCompiler___opt_compdir_t)(val_t p0);
static const char * const LOCATE_nitc___NitCompiler___opt_extension_prefix = "nitc::NitCompiler::opt_extension_prefix";
val_t nitc___NitCompiler___opt_extension_prefix(val_t p0);
typedef val_t (*nitc___NitCompiler___opt_extension_prefix_t)(val_t p0);
static const char * const LOCATE_nitc___NitCompiler___opt_output_format = "nitc::NitCompiler::opt_output_format";
val_t nitc___NitCompiler___opt_output_format(val_t p0);
typedef val_t (*nitc___NitCompiler___opt_output_format_t)(val_t p0);
static const char * const LOCATE_nitc___NitCompiler___init = "nitc::NitCompiler::init";
void nitc___NitCompiler___init(val_t p0, int* init_table);
typedef void (*nitc___NitCompiler___init_t)(val_t p0, int* init_table);
val_t NEW_NitCompiler_nitc___NitCompiler___init();
static const char * const LOCATE_nitc___NitCompiler___process_options = "nitc::NitCompiler::(mmloader::ToolContext::process_options)";
void nitc___NitCompiler___process_options(val_t p0);
typedef void (*nitc___NitCompiler___process_options_t)(val_t p0);
static const char * const LOCATE_nitc___NitCompiler___perform_work = "nitc::NitCompiler::(abstracttool::AbstractCompiler::perform_work)";
void nitc___NitCompiler___perform_work(val_t p0, val_t p1);
typedef void (*nitc___NitCompiler___perform_work_t)(val_t p0, val_t p1);
static const char * const LOCATE_nitc___Sys___main = "nitc::Sys::(kernel::Sys::main)";
void nitc___Sys___main(val_t p0);
typedef void (*nitc___Sys___main_t)(val_t p0);
val_t NEW_Sys_kernel___Sys___init();
#endif
