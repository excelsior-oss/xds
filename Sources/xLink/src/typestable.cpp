#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <stddef.h>
#include <ctype.h>

#include <vector>
#include <set>
#include <map>
#include <algorithm>

#include "xdefs.h"

open_namespace

#include "typestable.h"
#include "idents.h"
#include "messages.h"
#include "xdebug.h"
#include "xmem.h"
#include "struct.h"
#include "jet.h"

using namespace std;

/*----------------------------------------------------------------------------*/
/* Type entries.                                                              */
/*----------------------------------------------------------------------------*/


class TypeEntry : public ForeverObject {
    ident name;
    TypeEntryKind kind;
    int hash;

public:
    TypeEntry()
        : name(EMPTY_ID), kind(TDEntry_Invalid), hash(0)
    {}

    TypeEntry(ident name_, TypeEntryKind kind_, int hash_)
        : name(name_), kind(kind_), hash(hash_)
    {}

    ident getName() const {
        ASSERT (kind != TDEntry_Invalid);
        return name;
    }

    TypeEntryKind getKind() const {
        ASSERT (kind != TDEntry_Invalid);
        return kind;
    }

    void setKind(TypeEntryKind kind_) {
        ASSERT ((kind = TDEntry_Unknown) || (kind == kind_));
        kind = kind_;
    }

    int getHash() const {
        return hash;
    }

    void setHash(int hash_) {
        ASSERT ((hash == 0) || (hash == hash_));
        hash = hash_;
    }

    bool isArrayType() const;

    int getDimensions() const;

    ident getElemType() const;
};


bool TypeEntry::isArrayType() const {
    return (NAMES.Index2Str(name)[0] == '[');
}

int TypeEntry::getDimensions() const {
    const char* nm = NAMES.Index2Str(name);
    int dim = 0;
    while (nm[dim] == '[') {
        dim++;
    }
    ASSERT (dim > 0);
    return dim;
}

ident TypeEntry::getElemType() const {
    const char* nm = NAMES.Index2Str(name);
    ASSERT (*nm == '[');  // applicable for array types only

    while (*nm == '[') {
        nm++;
    }
    return NAMES.Str2Index(nm);
}


/*----------------------------------------------------------------------------*/
/* Global types table containing all types of the component.                  */
/*----------------------------------------------------------------------------*/

class RealTypesTable;

class GlobalTypesTable : public ForeverObject {
    typedef std::map<ident, TypeEntry*> TypeEntries;
    TypeEntries types;

    bool  arraysPresent;

    std::vector<LocalTypesTable*> localTables;
    std::vector<RealTypesTable*>  realTables;

public:
    GlobalTypesTable();

    LocalTypesTable* newLocalTable(const char* filename);
    void registerTypeEntry (ident name, TypeEntryKind kind, int hash = 0);

    const TypeEntry* getTypeEntry(ident name) const;

    void setTypeKinds();
    void createArrayTDs();
    void formRealTypesTables();
    void printRealTypesTables();
};

static GlobalTypesTable* globalTypesTable;

/*----------------------------------------------------------------------------*/
/* Types Table Order                                                          */
/*----------------------------------------------------------------------------*/

/* Ordering of types table:   
      (0                    .. arrTDs-1)               - Array descriptors
      (arrTDs               .. arrTDs+impTDs-1)        - Imported Type descriptors
      (arrTDs+impTDs        .. arrTDs+impTDs+expTDs-1) - Exported Type descriptors, ordered by hash
      (arrTDs+impTDs+expTDs .. nTDs-1)                 - Absent Type descriptors

      nTDs = arrTDs+impTDs+expTDs+absTDs
*/

class TypesTableOrder {
public:
    bool operator () (const TypeEntry* const& e1, const TypeEntry* const& e2) const {
        TypeEntryKind k1 = e1->getKind();
        TypeEntryKind k2 = e2->getKind();

        if (k1 != k2) {
            return k1 < k2;
        }
        if (k1 == TDEntry_Exported) {
            int hash1 = e1->getHash();
            int hash2 = e2->getHash();
            if (hash1 != hash2) {
                return hash1 < hash2;
            }
        }
        return NAMES.Compare(e1->getName(), e2->getName()) < 0;
    }
};


/*----------------------------------------------------------------------------*/
/* Real types table constructed by linker                                     */
/*----------------------------------------------------------------------------*/

class RealTypesTable : public ForeverObject {
    typedef std::map<ident, int> TypeIdx;

    std::vector<const TypeEntry*> table;
    TypeIdx typeIdx;
    int tableBase;

    int   arrTDs;
    int   impTDs;
    int   expTDs;
    int   absTDs;

    dword offset;

public:
    RealTypesTable()
        : tableBase(0),
          arrTDs(0),
          impTDs(0),
          expTDs(0),
          absTDs(0),
          offset(0)
    {}

    void setTableBase(int tableBase_) {
        tableBase = tableBase_;
    }

    int getTableBase() const {
        return tableBase;
    }

    int getTableLen() const {
        return 1 + table.size();
    }

    void setOffset(dword offset_) {
        offset = offset_;
    }

    dword getOffset() const {
        return offset;
    }

    int getNFixups(bool isLast) const {
        return (isLast ? 2 : 3) + // header
               arrTDs + impTDs + expTDs;
    }

    bool contains (ident name) const;

    bool canAddTypes(const IDSet& typesToAdd) const;
    void addLocalTable(LocalTypesTable* table);
    void sort();
    void calcNumTDs();
    void create(Segment* seg, bool isLast);
    void print() const;

    int getTDIndex16(ident name) const;
    int getTDIndex32(ident name) const;
};


bool RealTypesTable::contains (ident name) const {
    return (typeIdx.find(name) != typeIdx.end());
}


bool RealTypesTable::canAddTypes(const IDSet& typesToAdd) const {
    if (xSplitTypesTable && (typeIdx.size() != 0)) {
        return false;
    }

    int numToAdd = 0;
    for (IDSet::const_iterator it = typesToAdd.begin(); it != typesToAdd.end(); ++it) {
        ident t = *it;
        if (!contains(t)) {
            numToAdd++;
        }
    }

    return ((getTableLen() + numToAdd) <= MAX_TDINDEX16);
}


void RealTypesTable::addLocalTable(LocalTypesTable* localTable) {
    ASSERT (globalTypesTable != NULL);

    const IDSet& typesToAdd = localTable->getTypes();

    for (IDSet::const_iterator it = typesToAdd.begin(); it != typesToAdd.end(); ++it) {
        ident t = *it;

        if (!contains(t)) {
            table.push_back(globalTypesTable->getTypeEntry(t));
            typeIdx[t] = -1; // unassigned index
        }
    }
    ASSERT (getTableLen() <= MAX_TDINDEX16);

    localTable->setRealTable(this);
}


void RealTypesTable::sort() {
    std::stable_sort(table.begin(), table.end(), TypesTableOrder());

    for (size_t i = 0; i < table.size(); i++) {
        const TypeEntry* t = table[i];
        typeIdx[t->getName()] = i + 1;
    }
}


void RealTypesTable::calcNumTDs() {
    for (size_t i = 0; i < table.size(); i++) {
        const TypeEntry* t = table[i];
        switch (t->getKind()) {
            case TDEntry_Array:    arrTDs++; break;
            case TDEntry_Imported: impTDs++; break;
            case TDEntry_Exported: expTDs++; break;
            case TDEntry_Absent:   absTDs++; break;
            default:
                ASSERT_FALSE ();
        }
    }
}


/*
    Relationship between TypesTable & AbsentTypesTable:
    (TDINDEX16 - TypesTable.absentBase) = index in AbsentTypesTable

    TypesTable:  +---------+     AbsentTypesTable:  +---------+
                 |   ...   |                        |   ACD1  |
                 +---------+           +----------> |   ACD2  |
                 |     0x77|           |            |   ACD3  |
    TDINDEX16 -> |     0x77|  ---------+            |   ACD4  |
                 |     0x77|                        +---------+
                 |     0x77|
                 +---------+
*/

void RealTypesTable::create(Segment* seg, bool isLast) {
    dword offset = getOffset();

    ASSERT (offset + sizeof(struct TypesTable_Header) + getTableLen()*4 <= (dword) seg->getLen());
    struct TypesTable_Header* header = (struct TypesTable_Header*) (seg->getText() + offset);

    header->magic            = TypesTable_Magic;

    header->tableBase        = getTableBase();
    header->totalTDs         = getTableLen();
    header->nextTable        = NULL;
    header->initialized      = 0;

    header->arraysBase       = 1;
    header->arraysNum        = arrTDs;

    header->importedBase     = 1 + arrTDs;
    header->importedNum      = impTDs;

    header->exportedBase     = 1 + arrTDs + impTDs;
    header->exportedNum      = expTDs;
    header->hashIndex        = NULL;

    header->absentBase       = 1 + arrTDs + impTDs + expTDs;
    header->absentNum        = absTDs;
    header->absentTypesTable = NULL;

    if (!isLast) {
        // Link to next table
        addFixup (seg,
                  FIXUP_ADDRESS32,
                  offset + offsetof (struct TypesTable_Header, nextTable),
                  TK_SEG,
                  seg,
                  offset + sizeof(struct TypesTable_Header) + getTableLen()*4 + sizeof(struct TypesTable_Header));
    }

    // fill table

    int nTDs = arrTDs+impTDs+expTDs+absTDs;
    ASSERT (1 + nTDs == getTableLen());

    for (int i = 0; i < nTDs-absTDs; i++) {
        const TypeEntry* t = table[i];

        ASSERT (t->getKind() != TDEntry_Absent);

        addFixup (seg,
                  FIXUP_ADDRESS32,
                  offset + sizeof(struct TypesTable_Header) + (i + 1)*4,
                  TK_ID,
                  (void *) (t->getName()),
                  0);
    }

    for (int i = nTDs-absTDs; i < nTDs; i++) {
       *((dword *) (seg->getText() + offset + sizeof(struct TypesTable_Header) + (i + 1)*4)) = UNRESOLVED_TYPE;
    }

    // create hash index

    Segment * hashIndexSeg = new Segment (NAMES.Str2Index("Types Table Hash Index"), TD_DATA, DATA, false, expTDs*4, 4);
    for (int i = 0; i < expTDs; i++) {
        const TypeEntry* t = table[arrTDs + impTDs + i];
        ASSERT (t->getKind() == TDEntry_Exported);

        *((int *) (hashIndexSeg -> getText() + i*4)) = t->getHash();
    }

    addFixup (seg,
              FIXUP_ADDRESS32,
              offset + offsetof (struct TypesTable_Header, hashIndex),
              TK_SEG,
              hashIndexSeg,
              0);

    // create AbsentTypesTable

    Segment * absentTableSeg = new Segment (NAMES.Str2Index("Absent Types Table"), TD_DATA, DATA, false, absTDs*4, 4);
    absentTableSeg -> allocateFixups (absTDs);

    for (int i = 0; i < absTDs; i++) {
        const TypeEntry* t = table[arrTDs + impTDs + expTDs + i];
        ASSERT (t->getKind() == TDEntry_Absent);

        addFixup (absentTableSeg,
                  FIXUP_ADDRESS32,
                  i*4,
                  TK_ID,
                  (void *) (t->getName()),
                  0);
    }

    addFixup (seg,
              FIXUP_ADDRESS32,
              offset + offsetof (struct TypesTable_Header, absentTypesTable),
              TK_SEG,
              absentTableSeg,
              0);

    VerboseMessage(INFO_TYPEDESC, "TD: TypesTable created: %d TDs registered (%d/%d/%d/%d)\n", nTDs, arrTDs, impTDs, expTDs, absTDs);
}


void RealTypesTable::print() const {
    VerboseMessage (INFO_TYPEDESC, "TD: tableBase = %d\n", getTableBase());
    VerboseMessage (INFO_TYPEDESC, "TD: types = %d/%d/%d/%d\n", arrTDs, impTDs, expTDs, absTDs);
    for (size_t i = 0; i < table.size(); i++) {
        const TypeEntry* t = table[i];
        switch (t->getKind()) {
            case TDEntry_Array:
                VerboseMessage (INFO_TYPEDESC, "TD: TDTable [%d] (array) %s\n", i + 1, NAMES.Index2Str(t->getName()));
                break;

            case TDEntry_Imported:
                VerboseMessage (INFO_TYPEDESC, "TD: TDTable [%d] (imported) %s\n", i + 1, NAMES.Index2Str(t->getName()));
                break;

            case TDEntry_Exported:
                VerboseMessage (INFO_TYPEDESC, "TD: TDTable [%d] (exported) %s (hash=%d)\n", i + 1, NAMES.Index2Str(t->getName()), t->getHash());
                break;

            case TDEntry_Absent:
                VerboseMessage (INFO_TYPEDESC, "TD: TDTable [%d] (absent) %s\n", i + 1, NAMES.Index2Str(t->getName()));
                break;

            default:
                ASSERT_FALSE ();
        }
    }
}


int RealTypesTable::getTDIndex16(ident name) const {
    TypeIdx::const_iterator it = typeIdx.find(name);
    ASSERT (it != typeIdx.end());

    int tdindex = it->second;
    ASSERT ((tdindex > 0) && (tdindex < getTableLen()));

    return tdindex;
}


int RealTypesTable::getTDIndex32(ident name) const {
    return getTableBase() + getTDIndex16(name);
}




/*----------------------------------------------------------------------------*/
/* Local types table for each module.                                         */
/*----------------------------------------------------------------------------*/

LocalTypesTable::LocalTypesTable(const char* filename_, ident name_)
    : realTable(NULL), name(name_)

{
    filename = dup2AF(filename_);
    VerboseMessage(INFO_TYPEDESC, "TD: Local TypesTable %s created\n", NAMES.Index2Str(name));
}


void LocalTypesTable::registerTypeEntry (ident name, TypeEntryKind kind, int hash) {
    ASSERT (globalTypesTable != NULL);
    globalTypesTable->registerTypeEntry(name, kind, hash);

    types.insert(name);
    if (1 + types.size() > MAX_TDINDEX16) {
        Message (xFATAL, msgTOO_MANY_TYPES, getFileName());
    }

    VerboseMessage(INFO_TYPEDESC, "TD: Type %s registered in the local TypesTable %s\n", NAMES.Index2Str(name), NAMES.Index2Str(getTableName()));
}

dword LocalTypesTable::getOffset() const {
    ASSERT (realTable != NULL);
    return realTable->getOffset();
}

int LocalTypesTable::getTDIndex16(ident name) const {
    ASSERT (realTable != NULL);
    ASSERT (types.find(name) != types.end());
    return realTable->getTDIndex16(name);
}

int LocalTypesTable::getTDIndex32(ident name) const {
    ASSERT (realTable != NULL);
    ASSERT (types.find(name) != types.end());
    return realTable->getTDIndex32(name);
}


/*----------------------------------------------------------------------------*/
/* Global Types Table Implementation                                          */
/*----------------------------------------------------------------------------*/


GlobalTypesTable::GlobalTypesTable()
    : arraysPresent(false)
{
    xJetComponent = true;
}


LocalTypesTable* GlobalTypesTable::newLocalTable(const char* filename) {
    char nameBuf[64];
    sprintf (nameBuf, "LINK_LocalTypesTable%d", localTables.size());
    ident name = NAMES.Str2Index(nameBuf);

    LocalTypesTable* table = new LocalTypesTable(filename, name);
    localTables.push_back(table);

    return table;
}


void GlobalTypesTable::registerTypeEntry (ident name, TypeEntryKind kind, int hash) {
    TypeEntries::iterator it = types.find(name);
    if (it != types.end()) {
        TypeEntry* t = it->second;
        ASSERT (t->getName() == name);

        if (hash != 0) {
            t->setHash(hash);
        }
        if (kind != TDEntry_Unknown) {
            t->setKind(kind);
        }
        return;
    }

    types[name] = new TypeEntry(name, kind, hash);

    VerboseMessage (INFO_TYPEDESC, "TD REGISTERED: %s (hash = %d)\n", NAMES.Index2Str(name), hash);
}


const TypeEntry* GlobalTypesTable::getTypeEntry(ident name) const {
    TypeEntries::const_iterator it = types.find(name);
    ASSERT (it != types.end());
    return it->second;
}


/*
 * Initializes kinds of type entries.
 * Calculates number of types entries of each kind.
 */
void GlobalTypesTable::setTypeKinds() {

    for (TypeEntries::iterator it = types.begin(); it != types.end(); ++it) {
        TypeEntry* entry = it->second;

        if ((entry->getKind() == TDEntry_Absent) ||
            (entry->getKind() == TDEntry_Exported))
        {
            continue;
        }

        ASSERT (entry->getKind() == TDEntry_Unknown);

        if (entry->isArrayType()) {
            arraysPresent = true;

            ident elemTypeID = entry->getElemType();

            TypeEntries::const_iterator it = types.find(elemTypeID);
            if ((it != types.end()) && (it->second->getKind() == TDEntry_Absent)) {
                // array of absent type is also absent
                entry->setKind(TDEntry_Absent);
            } else {
                entry->setKind(TDEntry_Array);
            }
        } else {
            nameInfo * td = (nameInfo *) (NAMES.getInfo (entry->getName()));
            if (!td) {
                Message (xERROR, msgNAME_NOT_FOUND, NAMES.Index2Str (entry->getName()));
                continue;
            }
            if (td -> kind & K_IMPORT) {
                entry->setKind(TDEntry_Imported);
            } else {
                entry->setKind(TDEntry_Exported);
            }
        }
    }
}


/*
 * Generate Array TDs.
 */
void GlobalTypesTable::createArrayTDs() {

    if (arraysPresent)
        new OBJFile ("ARRAY TYPEDESCRIPTORS");

    for (TypeEntries::iterator it = types.begin(); it != types.end(); ++it) {
        TypeEntry* entry = it->second;

        if (!entry->isArrayType()) {
            continue;
        }

        int dim = entry->getDimensions();
        ASSERT (dim > 0);
        ASSERT (dim <= 255);

        ident elemTypeID = entry->getElemType();

        nameInfo * elemTypeName = (nameInfo *) (NAMES.getInfo (elemTypeID));
        if (elemTypeName == NULL) {
           Message (xERROR, msgCANT_FIND_ARRAY_ELEMTYPE, NAMES.Index2Str(elemTypeID));
           continue;
        }

        Segment * arraySeg = new Segment (entry->getName(), TD_DATA, DATA, false, sizeof(struct refAD), 4);
        struct refAD *r = (struct refAD*) (arraySeg -> getText());
        r -> classObject = 0;
        r -> kind        = k_array;
        r -> elTCode     = 0;
        r -> dimnum      = (byte) dim;

        addFixup (arraySeg,
                  FIXUP_ADDRESS32,
                  offsetof (struct refAD, elemtype),
                  TK_ID,
                  (void *) elemTypeID,
                  0);

        NewPublicName (entry->getName(), arraySeg, 0, T_TYPEDESC);
        VerboseMessage (INFO_TYPEDESC, "TD: Array descriptor created: dimnum = %d; elemtype = %s\n", dim, NAMES.Index2Str(elemTypeID));
    }
}


/*
 * Forms images of types tables.
 */
void GlobalTypesTable::formRealTypesTables() {
    RealTypesTable* realTable = new RealTypesTable();
    realTables.push_back(realTable);

    for (size_t i = 0; i < localTables.size(); i++) {
        LocalTypesTable* localTable = localTables[i];
        const IDSet& types = localTable->getTypes();

        if (!localTable->isTableReferenced()) {
            if (!types.empty()) {
                Message (xERROR, msgUNREFERENCED_TYPES_TABLE, localTable->getFileName());
            }
            continue;
        }

        if (!realTable->canAddTypes(types)) {
            realTable = new RealTypesTable();
            realTables.push_back(realTable);
            ASSERT (realTable->canAddTypes(types));
        }

        VerboseMessage (INFO_TYPEDESC, "TD: Local types table %s is put into real types table #%d\n", NAMES.Index2Str(localTable->getTableName()), realTables.size()-1);
        realTable->addLocalTable(localTable);
    }

    int tableBase = 0;
    int totalSize = 0;
    int nFixups = 0;

    ASSERT (sizeof(TypesTable_Header) % 4 == 0);

    for (size_t i = 0; i < realTables.size(); i++) {
        RealTypesTable* realTable = realTables[i];

        realTable->sort();
        realTable->calcNumTDs();
        realTable->setTableBase(tableBase);
        realTable->setOffset(totalSize);

        tableBase += realTable->getTableLen() +   // length of the current table
                     sizeof(TypesTable_Header)/4; // header of the next table

        totalSize += sizeof(TypesTable_Header) + realTable->getTableLen()*4;

        nFixups   += realTable->getNFixups(i == realTables.size()-1);
    }

    // Generate TD Table
    new OBJFile ("TYPES TABLE");

    Segment * tableSeg = new Segment (NAMES.Str2Index("Types Table"), TD_DATA, DATA, false, totalSize, 4);
    tableSeg->allocateFixups (nFixups);

    for (size_t i = 0; i < realTables.size(); i++) {
        RealTypesTable* realTable = realTables[i];
        realTable->create(tableSeg, (i == realTables.size()-1));
    }

    // Define public names for global and local types tables

    NewPublicName (NAMES.Str2Index("LINK_TypesTable"), tableSeg, sizeof(struct TypesTable_Header), T_DATA);

    for (size_t i = 0; i < localTables.size(); i++) {
        LocalTypesTable* localTable = localTables[i];
        NewPublicName (localTable->getTableName(), tableSeg, localTable->getOffset() + sizeof(struct TypesTable_Header), T_DATA);
    }
}


/*
 * Prints images of types tables.
 */
void GlobalTypesTable::printRealTypesTables() {
    for (size_t i = 0; i < realTables.size(); i++) {
        VerboseMessage (INFO_TYPEDESC, "-------------------- Real Types Table #%d --------------------\n", i);

        realTables[i]->print();

        VerboseMessage (INFO_TYPEDESC, "--------------------------------------------------------------\n");
        VerboseMessage (INFO_TYPEDESC, "\n");
    }
}


LocalTypesTable* newLocalTypesTable(const char* filename) {
    if (!globalTypesTable) {
        globalTypesTable = new GlobalTypesTable();
    }

    return globalTypesTable->newLocalTable(filename);
}


void PrepareTypesTable (void)
{
    if (globalTypesTable == NULL) {
        if (!xJetComponent) {
            return;
        }
        globalTypesTable = new GlobalTypesTable();
    }

    globalTypesTable->setTypeKinds ();
    if (TotalErrors != 0)
        return;

    globalTypesTable->createArrayTDs ();
    if (TotalErrors != 0)
        return;

    globalTypesTable->formRealTypesTables();

    if (IsPrintable (INFO_TYPEDESC)) {
        globalTypesTable->printRealTypesTables();
    }
}

/*----------------------------------------------------------------------------*/

close_namespace

