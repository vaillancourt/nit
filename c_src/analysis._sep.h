/* This C header file is generated by NIT to compile modules and programs that requires analysis. */
#ifndef analysis_sep
#define analysis_sep
#include "icode_dump._sep.h"
#include "inline_methods._sep.h"
#include "cha_analysis._sep.h"
#include "rta_analysis._sep.h"
#include "reachable_as_init_impl._sep.h"
#include "reachable_from_init_method_analysis_impl._sep.h"
#include "dead_method_removal._sep.h"
#include "inline_get_and_set._sep.h"
#include "remove_out_of_init_get_test._sep.h"
#include <nit_common.h>
extern const char *LOCATE_analysis;
extern const int SFT_analysis[];
#define ATTR_analysis___ToolContext____global_callgraph(recv) ATTR(recv, (SFT_analysis[0] + 0))
#define ATTR_analysis___ToolContext____no_dead_method_removal(recv) ATTR(recv, (SFT_analysis[0] + 1))
#define ATTR_analysis___ToolContext____no_inline_get_set(recv) ATTR(recv, (SFT_analysis[0] + 2))
#define ATTR_analysis___ToolContext____no_callgraph_from_init(recv) ATTR(recv, (SFT_analysis[0] + 3))
#define ATTR_analysis___ToolContext____no_out_of_init_get_test_removal(recv) ATTR(recv, (SFT_analysis[0] + 4))
#define CALL_analysis___ToolContext___global_callgraph(recv) ((analysis___ToolContext___global_callgraph_t)CALL((recv), (SFT_analysis[1] + 0)))
#define CALL_analysis___ToolContext___global_callgraph__eq(recv) ((analysis___ToolContext___global_callgraph__eq_t)CALL((recv), (SFT_analysis[1] + 1)))
#define CALL_analysis___ToolContext___no_dead_method_removal(recv) ((analysis___ToolContext___no_dead_method_removal_t)CALL((recv), (SFT_analysis[1] + 2)))
#define CALL_analysis___ToolContext___no_dead_method_removal__eq(recv) ((analysis___ToolContext___no_dead_method_removal__eq_t)CALL((recv), (SFT_analysis[1] + 3)))
#define CALL_analysis___ToolContext___no_inline_get_set(recv) ((analysis___ToolContext___no_inline_get_set_t)CALL((recv), (SFT_analysis[1] + 4)))
#define CALL_analysis___ToolContext___no_inline_get_set__eq(recv) ((analysis___ToolContext___no_inline_get_set__eq_t)CALL((recv), (SFT_analysis[1] + 5)))
#define CALL_analysis___ToolContext___no_callgraph_from_init(recv) ((analysis___ToolContext___no_callgraph_from_init_t)CALL((recv), (SFT_analysis[1] + 6)))
#define CALL_analysis___ToolContext___no_callgraph_from_init__eq(recv) ((analysis___ToolContext___no_callgraph_from_init__eq_t)CALL((recv), (SFT_analysis[1] + 7)))
#define CALL_analysis___ToolContext___no_out_of_init_get_test_removal(recv) ((analysis___ToolContext___no_out_of_init_get_test_removal_t)CALL((recv), (SFT_analysis[1] + 8)))
#define CALL_analysis___ToolContext___no_out_of_init_get_test_removal__eq(recv) ((analysis___ToolContext___no_out_of_init_get_test_removal__eq_t)CALL((recv), (SFT_analysis[1] + 9)))
#define CALL_analysis___Program___do_global_analysis(recv) ((analysis___Program___do_global_analysis_t)CALL((recv), (SFT_analysis[2] + 0)))
#define CALL_analysis___Program___do_global_pre_analysis_optimizations(recv) ((analysis___Program___do_global_pre_analysis_optimizations_t)CALL((recv), (SFT_analysis[2] + 1)))
#define CALL_analysis___Program___do_global_post_analysis_optimizations(recv) ((analysis___Program___do_global_post_analysis_optimizations_t)CALL((recv), (SFT_analysis[2] + 2)))
#define CALL_analysis___Program___dump_global_optimizations_information(recv) ((analysis___Program___dump_global_optimizations_information_t)CALL((recv), (SFT_analysis[2] + 3)))
#define CALL_analysis___Program___dump_global_analysis_information(recv) ((analysis___Program___dump_global_analysis_information_t)CALL((recv), (SFT_analysis[2] + 4)))
#define CALL_analysis___IRoutine___optimize(recv) ((analysis___IRoutine___optimize_t)CALL((recv), (SFT_analysis[3] + 0)))
static const char * const LOCATE_analysis___ToolContext___global_callgraph = "analysis::ToolContext::global_callgraph";
val_t analysis___ToolContext___global_callgraph(val_t p0);
typedef val_t (*analysis___ToolContext___global_callgraph_t)(val_t p0);
static const char * const LOCATE_analysis___ToolContext___global_callgraph__eq = "analysis::ToolContext::global_callgraph=";
void analysis___ToolContext___global_callgraph__eq(val_t p0, val_t p1);
typedef void (*analysis___ToolContext___global_callgraph__eq_t)(val_t p0, val_t p1);
static const char * const LOCATE_analysis___ToolContext___no_dead_method_removal = "analysis::ToolContext::no_dead_method_removal";
val_t analysis___ToolContext___no_dead_method_removal(val_t p0);
typedef val_t (*analysis___ToolContext___no_dead_method_removal_t)(val_t p0);
static const char * const LOCATE_analysis___ToolContext___no_dead_method_removal__eq = "analysis::ToolContext::no_dead_method_removal=";
void analysis___ToolContext___no_dead_method_removal__eq(val_t p0, val_t p1);
typedef void (*analysis___ToolContext___no_dead_method_removal__eq_t)(val_t p0, val_t p1);
static const char * const LOCATE_analysis___ToolContext___no_inline_get_set = "analysis::ToolContext::no_inline_get_set";
val_t analysis___ToolContext___no_inline_get_set(val_t p0);
typedef val_t (*analysis___ToolContext___no_inline_get_set_t)(val_t p0);
static const char * const LOCATE_analysis___ToolContext___no_inline_get_set__eq = "analysis::ToolContext::no_inline_get_set=";
void analysis___ToolContext___no_inline_get_set__eq(val_t p0, val_t p1);
typedef void (*analysis___ToolContext___no_inline_get_set__eq_t)(val_t p0, val_t p1);
static const char * const LOCATE_analysis___ToolContext___no_callgraph_from_init = "analysis::ToolContext::no_callgraph_from_init";
val_t analysis___ToolContext___no_callgraph_from_init(val_t p0);
typedef val_t (*analysis___ToolContext___no_callgraph_from_init_t)(val_t p0);
static const char * const LOCATE_analysis___ToolContext___no_callgraph_from_init__eq = "analysis::ToolContext::no_callgraph_from_init=";
void analysis___ToolContext___no_callgraph_from_init__eq(val_t p0, val_t p1);
typedef void (*analysis___ToolContext___no_callgraph_from_init__eq_t)(val_t p0, val_t p1);
static const char * const LOCATE_analysis___ToolContext___no_out_of_init_get_test_removal = "analysis::ToolContext::no_out_of_init_get_test_removal";
val_t analysis___ToolContext___no_out_of_init_get_test_removal(val_t p0);
typedef val_t (*analysis___ToolContext___no_out_of_init_get_test_removal_t)(val_t p0);
static const char * const LOCATE_analysis___ToolContext___no_out_of_init_get_test_removal__eq = "analysis::ToolContext::no_out_of_init_get_test_removal=";
void analysis___ToolContext___no_out_of_init_get_test_removal__eq(val_t p0, val_t p1);
typedef void (*analysis___ToolContext___no_out_of_init_get_test_removal__eq_t)(val_t p0, val_t p1);
val_t NEW_ToolContext_mmloader___ToolContext___init();
static const char * const LOCATE_analysis___Program___do_global_analysis = "analysis::Program::do_global_analysis";
void analysis___Program___do_global_analysis(val_t p0);
typedef void (*analysis___Program___do_global_analysis_t)(val_t p0);
static const char * const LOCATE_analysis___Program___do_global_pre_analysis_optimizations = "analysis::Program::do_global_pre_analysis_optimizations";
void analysis___Program___do_global_pre_analysis_optimizations(val_t p0);
typedef void (*analysis___Program___do_global_pre_analysis_optimizations_t)(val_t p0);
static const char * const LOCATE_analysis___Program___do_global_post_analysis_optimizations = "analysis::Program::do_global_post_analysis_optimizations";
void analysis___Program___do_global_post_analysis_optimizations(val_t p0);
typedef void (*analysis___Program___do_global_post_analysis_optimizations_t)(val_t p0);
static const char * const LOCATE_analysis___Program___dump_global_optimizations_information = "analysis::Program::dump_global_optimizations_information";
void analysis___Program___dump_global_optimizations_information(val_t p0, val_t p1);
typedef void (*analysis___Program___dump_global_optimizations_information_t)(val_t p0, val_t p1);
static const char * const LOCATE_analysis___Program___dump_global_analysis_information = "analysis::Program::dump_global_analysis_information";
void analysis___Program___dump_global_analysis_information(val_t p0, val_t p1);
typedef void (*analysis___Program___dump_global_analysis_information_t)(val_t p0, val_t p1);
val_t NEW_Program_program___Program___init(val_t p0, val_t p1);
static const char * const LOCATE_analysis___IRoutine___optimize = "analysis::IRoutine::optimize";
void analysis___IRoutine___optimize(val_t p0, val_t p1);
typedef void (*analysis___IRoutine___optimize_t)(val_t p0, val_t p1);
val_t NEW_IRoutine_icode_base___IRoutine___init(val_t p0, val_t p1);
#endif
