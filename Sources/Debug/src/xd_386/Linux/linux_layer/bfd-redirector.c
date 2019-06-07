/* Copyright (c) Excelsior. 2003. All Rights Reserved. */

/* BFD stuff. This module redirects several calls to bfd. */

#include "bfd-redirector.h"


bfd *
my_bfd_open (const char* name)
{
    return bfd_openr (name, 0);
}

int 
my_bfd_close (bfd* abfd)
{
    bfd_close (abfd);
}

int 
my_bfd_check_format (bfd* abfd)
{
    return bfd_check_format (abfd, bfd_object);
}

unsigned long 
my_bfd_get_startup_entry (bfd* abfd)
{
    return bfd_get_start_address (abfd);
}

int 
my_bfd_get_elf_phdr_upper_bound (bfd* abfd)
{
    return bfd_get_elf_phdr_upper_bound (abfd);
}

int 
my_bfd_get_elf_phdrs (bfd* abfd, void* phdrs)
{
    return bfd_get_elf_phdrs (abfd, phdrs);
/*
    int number = 0;
    int phdrs_number;
    struct elf_internal_phdr *pheader = phdrs;

    phdrs_number = bfd_get_elf_phdrs (abfd, phdrs);

    for (i = 0; i < phdrs_number; i++, pheader++)
      {
        if (pheader->p_type == PT_LOAD)
          {
            number++;
          }
      }
    return number;
/**/
}


int 
my_bfd_get_segment_info (struct elf_internal_phdr *phdrs, int index, unsigned long base,
                         int* start, int* end, int* flags)
{
    struct elf_internal_phdr *pheader = &phdrs[index];
    int mask = pheader->p_align - 1;
    int tmp;

    if(pheader->p_memsz == 0) {
        *start = *end = *flags = 0;
        return 0;
    }

    *start = tmp = (pheader->p_vaddr & ~mask) + base;
    *end = tmp - 1 + ((pheader->p_memsz + mask) & ~mask);
    *flags = pheader->p_flags;

    tmp = pheader->p_type == PT_LOAD;

//    printf ("bfd: segment info: start=%#8x, end=%#8x, flags=%d, is_loadable=%d\n", 
//      *start, *end, *flags, tmp);

    return tmp;
/*
    while (1) {
        pheader++;

        if (pheader->p_type == PT_NULL) {
            return 0;
        }

        else if (pheader->p_type == PT_LOAD) {
            return pheader;
        }
    }
    return 0;
*/
}

int 
my_bfd_get_symtab_upper_bound (bfd* abfd)
{
    return bfd_get_symtab_upper_bound (abfd);
}

int 
my_bfd_canonicalize_symtab (bfd* abfd, void* buffer)
{
    return bfd_canonicalize_symtab (abfd, buffer);
}

void 
my_bfd_get_symbol_info (asymbol* symbol_table[], int index, int* symaddr, char* name)
{
    asymbol *sym = symbol_table[index];
    *symaddr = sym->value + sym->section->vma;
    strcpy (name, sym->name);
/*
    if ( *name == 0) {
        strcpy (name, bfd_get_section_name (0, bfd_get_section (sym)));
    }
*/
//    printf ("symbol: '%s', symaddr=%#x\n", name, *symaddr);
}


int 
my_bfd_get_section_number (bfd* abfd)
{
    return bfd_count_sections (abfd);
}

asection* 
my_bfd_get_first_section (bfd* abfd)
{
    return abfd->sections;
}

asection* 
my_bfd_get_next_section (asection* sect)
{
    return sect->next;
}

void 
my_bfd_get_section_info (asection* sect, unsigned long base, int* start, int* end, int* is_data)
{
    int tmp;
    char **segm_names;
    bfd *abfd = sect->owner;

    *start = tmp = bfd_section_vma (abfd, sect) + base;
    *end = tmp + bfd_section_size (abfd, sect) - 1;
    *is_data = 1;

    for (segm_names = text_segment_names; *segm_names != 0; segm_names++) {
        if (strcmp (bfd_section_name (abfd, sect), *segm_names) == 0) {
            *is_data = 0;
            break;
        }
    }

//    printf ("section info: start=%#8x, end=%#8x, is_data=%d, name=%s\n", 
//      *start, *end, *is_data, bfd_section_name (abfd, sect));
}


int 
bfd_lookup_symbol (bfd *abfd, char *symname)
{
    long storage_needed;
    asymbol *sym;
    asymbol **symbol_table;
    asymbol **psym;
    unsigned int number_of_symbols;
    unsigned int i;
    int symaddr = 0;

    storage_needed = bfd_get_symtab_upper_bound (abfd);

    if (storage_needed > 0) {
        psym = symbol_table = (asymbol **) malloc (storage_needed);
        number_of_symbols = bfd_canonicalize_symtab (abfd, symbol_table);

        for (i = 0; i < number_of_symbols; i++) {
            sym = *psym++;
            if (strcmp (sym->name, symname) == 0) {
                // Bfd symbols are section relative.
                symaddr = sym->value + sym->section->vma;
                break;
            }
        }
        free (symbol_table);
    }

    if (symaddr) {
        return symaddr;
    }

    // On FreeBSD, the dynamic linker is stripped by default.  So we'll
    // have to check the dynamic string table too.

    storage_needed = bfd_get_dynamic_symtab_upper_bound (abfd);

    if (storage_needed > 0) {
        psym = symbol_table = (asymbol **) malloc (storage_needed);
        number_of_symbols = bfd_canonicalize_dynamic_symtab (abfd, symbol_table);

        for (i = 0; i < number_of_symbols; i++) {
            sym = *psym++;
            if (strcmp (sym->name, symname) == 0) {
                // Bfd symbols are section relative. 
                symaddr = sym->value + sym->section->vma;
                break;
            }
        }
        free (symbol_table);
    }
    return symaddr;
}

