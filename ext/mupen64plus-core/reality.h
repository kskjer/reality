#pragma once

#include <stdint.h>

#define REALITY_MUPEN_EXPORTS(X)														 \
	X(trainer_init,      (void *, void *, uint32_t, void *, void *, void *), void *)	 \
	X(trainer_deinit,    (void **),                                          void  )	 \
	X(trainer_insn_pre,  (void *, uint32_t),                                 void  )	 \
	X(trainer_insn_post, (void *, uint32_t),                                 void  )	 \
	X(trainer_vblank,    (void *),                                           void  )	 \
	X(trainer_dma,       (void *, uint32_t, uint32_t, uint32_t, uint32_t),   void  )	 \

#define X(name, args, ret)	extern ret (*name) args;
REALITY_MUPEN_EXPORTS(X)
#undef X

extern void load_reality_mupen_library();
extern void *reality_state;