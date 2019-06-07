/* Copyright (c) Excelsior. 2003. All Rights Reserved. */

/* BFD stuff. This module redirects several calls to bfd. */

#ifndef _BFD_REDIRECTOR_H_
#define _BFD_REDIRECTOR_H_

#include <bfd.h>


// TODO: write descriptive comments for each function


/* The BFD functions. */

extern bfd *my_bfd_open (const char* name);

extern int my_bfd_close (bfd* abfd);

extern int my_bfd_check_format (bfd* abfd);

extern unsigned long my_bfd_get_startup_entry (bfd* abfd);

/**
 * Taken from binutils/include/elf/internals.h
 * This structure may change in next bfd release
 */
struct elf_internal_phdr {
    unsigned long p_type;                 /* Identifies program segment type */
    unsigned long p_flags;                /* Segment flags */
    bfd_vma       p_offset;               /* Segment file offset */
    bfd_vma       p_vaddr;                /* Segment virtual address */
    bfd_vma       p_paddr;                /* Segment physical address */
    bfd_vma       p_filesz;               /* Segment size in file */
    bfd_vma       p_memsz;                /* Segment size in memory */
    bfd_vma       p_align;                /* Segment alignment, file & memory */
};

extern int my_bfd_get_elf_phdr_upper_bound (bfd* abfd);

extern int my_bfd_get_elf_phdrs (bfd* abfd, void* phdrs);


extern int my_bfd_get_segment_info (struct elf_internal_phdr *phdrs, 
                                    int index, unsigned long base,
                                    int* start, int* end, int* flags);

extern int my_bfd_get_symtab_upper_bound (bfd* abfd);

extern int my_bfd_canonicalize_symtab (bfd* abfd, void* buffer);

extern void my_bfd_get_symbol_info (asymbol* symbol_table[], int index, 
                                    int* symaddr, char* name);


extern int my_bfd_get_section_number (bfd* abfd);

extern asection* my_bfd_get_first_section (bfd* abfd);

extern asection* my_bfd_get_next_section (asection* sect);

extern void my_bfd_get_section_info (asection* sect, unsigned long base, int* start, 
                                     int* end, int* is_data);

extern int bfd_lookup_symbol (bfd *abfd, char *symname);


#endif // _BFD_REDIRECTOR_H_
