
#include "HsFFI.h"
#include "voidp_dll_stub.h"
#include <stdio.h>


extern "C" {q
    void HsStart();
    void HsEnd();
}


int main(int argc, char *argv[])
{
  if (argc !=6){
    printf("usage: total should have 5 inputs, hmm_prior.txt, hmm_transmat.txt, hmm_obsmat.txt, dimension, selection_num");
    return(0);
  }
    
    HsStart();

    // can now safely call functions from the DLL
    //printf("selections: %s\n", voidp())    ;
    char *s1 = argv[1]; //"hmm_prior.txt";
    char *s2 = argv[2]; //"hmm_transmat.txt";
    char *s3 = argv[3]; //"hmm_obsmat.txt";
    char *s4 = argv[4]; //"24";
    char *s5= argv[5];  //"10";
    voidp(s1,s2,s3,s4,s5);
    HsEnd();
    return 0;
}
