// StartEnd.c
#include <Rts.h>

extern void __stginit_Voidp(void);

void HsStart()
{
   int argc = 1;
   char* argv[] = {"ghcDll", NULL}; // argv must end with NULL

   // Initialize Haskell runtime
   char** args = argv;
   hs_init(&argc, &args);

   // Tell Haskell about all root modules
   hs_add_root(__stginit_Voidp);
}

void HsEnd()
{
   hs_exit();
}
