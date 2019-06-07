
#ifndef TYPESTABLE_H
#define TYPESTABLE_H

#include "xdefs.h"
#include "xmem.h"
#include "idents.h"

/*----------------------------------------------------------------------------*/
/*                                Types Table                                 */
/*----------------------------------------------------------------------------*/

/*
 * Maximum value for 16-bit type index in local types table.
 */
#define MAX_TDINDEX16  0xFFFF


typedef enum {
    TDEntry_Invalid,
    TDEntry_Unknown,
    TDEntry_Array,
    TDEntry_Imported,
    TDEntry_Exported,
    TDEntry_Absent
} TypeEntryKind;


typedef std::set<ident> IDSet;

/*
 * Local types table for each module.
 */
class RealTypesTable;

class LocalTypesTable : public ForeverObject {
    char* filename;
    ident name;
    IDSet types;
    RealTypesTable* realTable;
    bool  referenced;

public:
    LocalTypesTable(const char* filename, ident name);

    void registerTypeEntry (ident name, TypeEntryKind kind, int hash = 0);

    const char* getFileName() const {
        return filename;
    }

    ident getTableName() const {
        return name;
    }

    const IDSet& getTypes() const {
        return types;
    }

    void setRealTable(RealTypesTable* table) {
        ASSERT (realTable == NULL);
        realTable = table;
    }

    void setTableReferenced() {
        referenced = true;
    }

    bool isTableReferenced() const {
        return referenced;
    }

    dword getOffset() const;
    int getTDIndex16(ident name) const;
    int getTDIndex32(ident name) const;
};

extern LocalTypesTable* newLocalTypesTable(const char* filename);

/*
 * Creates types tables.
 */
extern void PrepareTypesTable ();

#endif
