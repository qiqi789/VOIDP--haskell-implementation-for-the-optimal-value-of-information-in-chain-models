#define IN_STG_CODE 0
#include "Rts.h"
#include "Stg.h"
#ifdef __cplusplus
extern "C" {
#endif
 
extern StgClosure Voidp_zdfvoidpzua2q9_closure;
void voidp(HsPtr a1, HsPtr a2, HsPtr a3, HsPtr a4, HsPtr a5)
{
Capability *cap;
HaskellObj ret;
cap = rts_lock();
cap=rts_evalIO(cap,rts_apply(cap,(HaskellObj)runIO_closure,rts_apply(cap,rts_apply(cap,rts_apply(cap,rts_apply(cap,rts_apply(cap,&Voidp_zdfvoidpzua2q9_closure,rts_mkPtr(cap,a1)),rts_mkPtr(cap,a2)),rts_mkPtr(cap,a3)),rts_mkPtr(cap,a4)),rts_mkPtr(cap,a5))) ,&ret);
rts_checkSchedStatus("voidp",cap);
rts_unlock(cap);
}
static void stginit_export_Voidp_zdfvoidpzua2q9() __attribute__((constructor));
static void stginit_export_Voidp_zdfvoidpzua2q9()
{getStablePtr((StgPtr) &Voidp_zdfvoidpzua2q9_closure);}
#ifdef __cplusplus
}
#endif

