/***	Copyight (c) 1996 xTech Ltd. Russia    ***/
/*
   Magic generator for CTRout(CityRout) - Compiler Testing Routine
												   Alexs: 4-Nov-96
*/
#ifndef __ctMagicGen_h
#define __ctMagicGen_h

#define NO_MAGIC 0x9A8B7C6D
#define DeclareMagic(x) static long x = NO_MAGIC
#define SetMagic(x) 	(((x) == NO_MAGIC)?(SetMagic_F(&(x))):(x))

  extern long SetMagic_F(long * x);

#endif /* __ctMagicGen_h */
