/* This C file is generated by NIT to compile module environ. */
#include "environ._sep.h"
val_t environ___Symbol___environ(val_t p0){
  struct {struct stack_frame_t me; val_t MORE_REG[1];} fra;
  val_t REGB0;
  val_t REGB1;
  val_t tmp;
  static val_t once_value_1; /* Once value */
    static val_t once_value_2; /* Once value */
    static val_t once_value_4; /* Once value */
  fra.me.prev = stack_frame_head; stack_frame_head = &fra.me;
  fra.me.file = LOCATE_environ;
  fra.me.line = 19;
  fra.me.meth = LOCATE_environ___Symbol___environ;
  fra.me.has_broke = 0;
  fra.me.REG_size = 2;
  fra.me.REG[0] = NIT_NULL;
  fra.me.REG[1] = NIT_NULL;
  fra.me.REG[0] = p0;
  /* ./../lib/standard//environ.nit:22 */
  fra.me.REG[0] = CALL_string___Object___to_s(fra.me.REG[0])(fra.me.REG[0]);
  fra.me.REG[0] = CALL_string___String___to_cstring(fra.me.REG[0])(fra.me.REG[0]);
  fra.me.REG[0] = CALL_environ___NativeString___get_environ(fra.me.REG[0])(fra.me.REG[0]);
  /* ./../lib/standard//environ.nit:24 */
  if (!once_value_1) {
    if (!once_value_2) {
      fra.me.REG[1] = BOX_NativeString("");
      REGB0 = TAG_Int(0);
      fra.me.REG[1] = NEW_String_string___String___with_native(fra.me.REG[1], REGB0);
      once_value_2 = fra.me.REG[1];
      register_static_object(&once_value_2);
    } else fra.me.REG[1] = once_value_2;
    fra.me.REG[1] = fra.me.REG[1];
    fra.me.REG[1] = CALL_string___String___to_cstring(fra.me.REG[1])(fra.me.REG[1]);
    fra.me.REG[1] = CALL_environ___NativeString___get_environ(fra.me.REG[1])(fra.me.REG[1]);
    once_value_1 = fra.me.REG[1];
    register_static_object(&once_value_1);
  } else fra.me.REG[1] = once_value_1;
  fra.me.REG[1] = fra.me.REG[1];
  /* ./../lib/standard//environ.nit:25 */
  REGB0 = TAG_Bool(IS_EQUAL_OO(fra.me.REG[0],fra.me.REG[1]));
  if (UNTAG_Bool(REGB0)) {
  } else {
    REGB1 = CALL_kernel___Object_____eqeq(fra.me.REG[0])(fra.me.REG[0], fra.me.REG[1]);
    REGB0 = REGB1;
  }
  REGB0 = TAG_Bool(!UNTAG_Bool(REGB0));
  if (UNTAG_Bool(REGB0)) {
    /* ./../lib/standard//environ.nit:26 */
    fra.me.REG[0] = NEW_String_string___String___from_cstring(fra.me.REG[0]);
    goto label3;
  } else {
    /* ./../lib/standard//environ.nit:28 */
    if (!once_value_4) {
      fra.me.REG[1] = BOX_NativeString("");
      REGB0 = TAG_Int(0);
      fra.me.REG[1] = NEW_String_string___String___with_native(fra.me.REG[1], REGB0);
      once_value_4 = fra.me.REG[1];
      register_static_object(&once_value_4);
    } else fra.me.REG[1] = once_value_4;
    fra.me.REG[1] = fra.me.REG[1];
    fra.me.REG[0] = fra.me.REG[1];
    goto label3;
  }
  label3: while(0);
  stack_frame_head = fra.me.prev;
  return fra.me.REG[0];
}
val_t environ___NativeString___get_environ(val_t p0){
  struct {struct stack_frame_t me;} fra;
  val_t tmp;
  fra.me.prev = stack_frame_head; stack_frame_head = &fra.me;
  fra.me.file = LOCATE_environ;
  fra.me.line = 34;
  fra.me.meth = LOCATE_environ___NativeString___get_environ;
  fra.me.has_broke = 0;
  fra.me.REG_size = 1;
  fra.me.REG[0] = NIT_NULL;
  fra.me.REG[0] = p0;
  /* ./../lib/standard//environ.nit:34 */
  fra.me.REG[0] = BOX_NativeString(string_NativeString_NativeString_get_environ_0(UNBOX_NativeString(fra.me.REG[0])));
  stack_frame_head = fra.me.prev;
  return fra.me.REG[0];
}
