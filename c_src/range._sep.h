/* This C header file is generated by NIT to compile modules and programs that requires range. */
#ifndef range_sep
#define range_sep
#include "abstract_collection._sep.h"
#include <nit_common.h>

extern const classtable_elt_t VFT_Range[];

extern const classtable_elt_t VFT_IteratorRange[];
extern const char *LOCATE_range;
extern const int SFT_range[];
#define ID_Range (SFT_range[0])
#define COLOR_Range (SFT_range[1])
#define ATTR_range___Range____first(recv) ATTR(recv, (SFT_range[2] + 0))
#define ATTR_range___Range____last(recv) ATTR(recv, (SFT_range[2] + 1))
#define ATTR_range___Range____after(recv) ATTR(recv, (SFT_range[2] + 2))
#define INIT_TABLE_POS_Range (SFT_range[3] + 0)
#define CALL_range___Range___last(recv) ((range___Range___last_t)CALL((recv), (SFT_range[3] + 1)))
#define CALL_range___Range___after(recv) ((range___Range___after_t)CALL((recv), (SFT_range[3] + 2)))
#define CALL_range___Range___init(recv) ((range___Range___init_t)CALL((recv), (SFT_range[3] + 3)))
#define CALL_range___Range___without_last(recv) ((range___Range___without_last_t)CALL((recv), (SFT_range[3] + 4)))
#define ID_IteratorRange (SFT_range[4])
#define COLOR_IteratorRange (SFT_range[5])
#define ATTR_range___IteratorRange____range(recv) ATTR(recv, (SFT_range[6] + 0))
#define ATTR_range___IteratorRange____item(recv) ATTR(recv, (SFT_range[6] + 1))
#define INIT_TABLE_POS_IteratorRange (SFT_range[7] + 0)
#define CALL_range___IteratorRange___init(recv) ((range___IteratorRange___init_t)CALL((recv), (SFT_range[7] + 1)))
static const char * const LOCATE_range___Range___first = "range::Range::(abstract_collection::Collection::first)";
val_t range___Range___first(val_t p0);
typedef val_t (*range___Range___first_t)(val_t p0);
static const char * const LOCATE_range___Range___last = "range::Range::last";
val_t range___Range___last(val_t p0);
typedef val_t (*range___Range___last_t)(val_t p0);
static const char * const LOCATE_range___Range___after = "range::Range::after";
val_t range___Range___after(val_t p0);
typedef val_t (*range___Range___after_t)(val_t p0);
static const char * const LOCATE_range___Range___has = "range::Range::(abstract_collection::Collection::has)";
val_t range___Range___has(val_t p0, val_t p1);
typedef val_t (*range___Range___has_t)(val_t p0, val_t p1);
static const char * const LOCATE_range___Range___has_only = "range::Range::(abstract_collection::Collection::has_only)";
val_t range___Range___has_only(val_t p0, val_t p1);
typedef val_t (*range___Range___has_only_t)(val_t p0, val_t p1);
static const char * const LOCATE_range___Range___count = "range::Range::(abstract_collection::Collection::count)";
val_t range___Range___count(val_t p0, val_t p1);
typedef val_t (*range___Range___count_t)(val_t p0, val_t p1);
static const char * const LOCATE_range___Range___iterator = "range::Range::(abstract_collection::Collection::iterator)";
val_t range___Range___iterator(val_t p0);
typedef val_t (*range___Range___iterator_t)(val_t p0);
typedef void (*CLOS_range___Range___iterate_0)(struct stack_frame_t *, val_t);
static const char * const LOCATE_range___Range___iterate = "range::Range::(abstract_collection::Collection::iterate)";
void range___Range___iterate(val_t p0, struct stack_frame_t *closctx_param, fun_t clos_fun0);
typedef void (*range___Range___iterate_t)(val_t p0, struct stack_frame_t *closctx_param, fun_t clos_fun0);
static const char * const LOCATE_range___Range___length = "range::Range::(abstract_collection::Collection::length)";
val_t range___Range___length(val_t p0);
typedef val_t (*range___Range___length_t)(val_t p0);
static const char * const LOCATE_range___Range___is_empty = "range::Range::(abstract_collection::Collection::is_empty)";
val_t range___Range___is_empty(val_t p0);
typedef val_t (*range___Range___is_empty_t)(val_t p0);
static const char * const LOCATE_range___Range___init = "range::Range::init";
void range___Range___init(val_t p0, val_t p1, val_t p2, int* init_table);
typedef void (*range___Range___init_t)(val_t p0, val_t p1, val_t p2, int* init_table);
val_t NEW_Range_range___Range___init(val_t p0, val_t p1);
static const char * const LOCATE_range___Range___without_last = "range::Range::without_last";
void range___Range___without_last(val_t p0, val_t p1, val_t p2, int* init_table);
typedef void (*range___Range___without_last_t)(val_t p0, val_t p1, val_t p2, int* init_table);
val_t NEW_Range_range___Range___without_last(val_t p0, val_t p1);
static const char * const LOCATE_range___IteratorRange___item = "range::IteratorRange::(abstract_collection::Iterator::item)";
val_t range___IteratorRange___item(val_t p0);
typedef val_t (*range___IteratorRange___item_t)(val_t p0);
static const char * const LOCATE_range___IteratorRange___is_ok = "range::IteratorRange::(abstract_collection::Iterator::is_ok)";
val_t range___IteratorRange___is_ok(val_t p0);
typedef val_t (*range___IteratorRange___is_ok_t)(val_t p0);
static const char * const LOCATE_range___IteratorRange___next = "range::IteratorRange::(abstract_collection::Iterator::next)";
void range___IteratorRange___next(val_t p0);
typedef void (*range___IteratorRange___next_t)(val_t p0);
static const char * const LOCATE_range___IteratorRange___init = "range::IteratorRange::init";
void range___IteratorRange___init(val_t p0, val_t p1, int* init_table);
typedef void (*range___IteratorRange___init_t)(val_t p0, val_t p1, int* init_table);
val_t NEW_IteratorRange_range___IteratorRange___init(val_t p0);
#endif
