
#ifndef OMF_H
#define OMF_H

/* The maximum OMF record size supported by OMF linkers.  This value
   includes the record type, length and checksum fields. */

#define MAX_REC_SIZE    1024

/* OMF record types.  To get the 32-bit variant of a record type, add
   REC32. */

#define THEADR          0x80    /* Translator module header record */
#define LHEADR          0x82    /* Library module header record */
#define COMENT          0x88    /* Comment record */
#define MODEND          0x8a    /* Module end record */
#define EXTDEF          0x8c    /* External names definition record */
#define TYPDEF          0x8e    /* Type definition record */
#define PUBDEF          0x90    /* Public names definition record */
#define LINNUM          0x94    /* Line numbers record */
#define LNAMES          0x96    /* List of names record */
#define SEGDEF          0x98    /* Segment definition record */
#define GRPDEF          0x9a    /* Group definition record */
#define FIXUPP          0x9c    /* Fixup record */
#define LEDATA          0xa0    /* Logical enumerated data record */
#define LIDATA          0xa2    /* Logical iterated data record */
#define COMDEF          0xb0    /* Communal names definition record */
#define BAKPAT          0xb2    /* Bacpatch record */
#define LEXTDEF         0xb4    /* Local external names definition record */
#define LPUBDEF         0xb6    /* Local public names definition record */
#define LCOMDEF         0xb8    /* Local communal names definition record */
#define CEXTDEF         0xbc    /* COMDAT external names definition record */
#define COMDAT          0xc2    /* Common block */
#define LINSYM          0xc4    /* Symbol line numbers record */
#define ALIAS           0xc6    /* Alias definition record */
#define NBKPAT          0xc8    /* Named backpatch record */
#define LLNAMES         0xca    /* Local logical names definition record */
#define VERNUM          0xcc    /* OMF version number record */
#define VENDEXT         0xce    /* Vendor-specific OMF extension record */
#define LIBHDR          0xf0    /* Library header */
#define LIBEND          0xf1    /* Library end */



/* Add this constant (using the | operator) to get the 32-bit variant
   of a record type.  Some fields will contain 32-bit values instead
   of 16-bit values. */

#define REC32           0x01


/* This is the layout of a relocation table entry. */

struct reloc
{
  dword address;                /* Fixup location */
  dword symbolnum:24;           /* Symbol number or segment */
  dword pcrel:1;                /* Self-relative fixup if non-zero */
  dword length:2;               /* Fixup size (0: 1 byte, 1: 2, 2: 4 bytes) */
  dword ext:1;                  /* Reference to symbol or segment */
  dword unused:4;               /* Not used */
};

#define MODNAM          0xC8    /* Module name */
#define FULNAM          0xC9    /* Fully qualified name */
#define VERSTAMP        0xCA    /* Version stamp */
#define IMPDEF          0x01

//  Frame kinds (obsolete)

#define FK_SEG          0
#define FK_GROUP        1
#define FK_ID           2
#define FK_CURSEG       4
#define FK_TARGET       5


//  OMF Fixup kinds

#define FX_SELFRELATIVE 128

#define FX_OFFSET32      9
#define FX_FAR16_16      3
#define FX_FAR16_32     11
#define FX_OFFSET32SR   FX_OFFSET32 | FX_SELFRELATIVE

#endif  /* OMF_H */
