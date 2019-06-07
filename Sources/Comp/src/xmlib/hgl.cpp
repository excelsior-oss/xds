
// HGL3.CPP (Version 3.0)

#include <fcntl.h>
#include <io.h>
#include <sys\stat.h>
#include <stdio.h>
#include "hgl.h"

void SetHiFrBit2(int fr);

unsigned const M_UNIT = 0xFFFFFFFF;

int GrType = 0;
unsigned int GGenOpt = 18434;
int Grid_w = 10;
int Grid_h = 10;
int Grid_x = 0; 
int Grid_y = 0;
int Ex = 0;
DinArr<Vertex>* Vertices;
DinArr<Fragment>* Fragments;
DinArr<Edge>* Edges;
Type* type;
int typ_num = 0;
Stack* EUStack;
int EUSwitch;
int EUNum;
ListBox* InEdges;
ListBox* OutEdges;
ListBox* GenLists;
ListBox* GenX;
ListBox* GenY;
ListBox* XBends;
ListBox* YBends;
StringBox* GenStrings;
int RRPar = 50;
LOGFONT DefLogFont;
LOGFONT DefLogFont2;
long Next_F_ID = 0;

int InitFrWidth = 16;
int InitFrHeight = 16;
int BB_Dist = 4;

PARAMINFO* param;
int par_num = 0;

int Vertex_Save_Size;
int Fragment_Save_Size;
int Edge_Save_Size;
int Type_Save_Size;
int Label_Save_Size;

template <class T> DinArr<T>::~DinArr()
{
    int b1 = num >> 8;
    int b0 = num & 255;
    for (int i=0; i<b1; i++) delete[](tab[i]);
    if (b0) delete[](tab[i]);
}

template <class T> int DinArr<T>::Add(T x)
{
    int b1 = num >> 8;
    int b0 = num & 255;
    if (!b0) tab[b1] = new T[256];
    tab[b1][b0] = x;
    num++; return num - 1;
}

template <class T> int DinArr<T>::Delete(int n)
{
    if (n >= num) return M_UNIT;
    num--;
    int b1 = num >> 8;
    int b0 = num & 255;
    int a1 = n >> 8;
    int a0 = n & 255;
    tab[a1][a0] = tab[b1][b0];
    if (!b0) delete[](tab[b1]);
    return num;
}

// ----------------------- ListBox---------------

ListBox::ListBox()
{
    fbound = 1;
    dbn = 1;
    data[0] = new ListIt[256];
    for (int i=0; i<256; i++) data[0][i].next = M_UNIT;
    empty_sign = M_UNIT;
    cur_el = 0;
    cstack = new Stack;
}

ListBox::~ListBox()
{
    for (unsigned int i=0; i<dbn; i++) delete[](data[i]);
    delete(cstack);
}

int ListBox::Get(LKEY key)
{
    cur_el = data[key>>8][key&255].next;
    return cur_el;
}

int ListBox::Next()
{
    if (!cur_el) return empty_sign;
    ListIt x = data[cur_el>>8][cur_el&255];
    cur_el = x.next;
    return x.n;
}

void ListBox::SetEmptySign(int sign)
{
    empty_sign = sign;
}

LKEY ListBox::NewList()
{
    if (fbound == dbn*256)
    {
        data[dbn] = new ListIt[256];
        for (int i=0; i<256; i++) data[dbn][i].next = M_UNIT;
        dbn++;
    }
    ListIt x;
    x.end = fbound;
    x.next = 0;
    data[fbound>>8][fbound&255] = x;
    do fbound++;
    while (fbound < dbn*256 && data[fbound>>8][fbound&255].next != M_UNIT);
    return x.n;
}

void ListBox::DestList(LKEY key)
{
    LKEY n;
    do
    {
        n = data[key>>8][key&255].next;
        data[key>>8][key&255].next = M_UNIT;
        if (key < fbound) fbound = key;
        key = n;
    }
    while (n);
}

void ListBox::AddFirst(LKEY key, int n)
{
    if (fbound == dbn*256)
    {
        data[dbn] = new ListIt[256];
        for (int i=0; i<256; i++) data[dbn][i].next = M_UNIT;
        dbn++;
    }
    ListIt x;
    x.n = n;
    x.next = data[key>>8][key&255].next;
    data[fbound>>8][fbound&255] = x;
    data[key>>8][key&255].next = fbound;
    if (data[key>>8][key&255].end == key) data[key>>8][key&255].end = fbound;
    do fbound++;
    while (fbound < dbn*256 && data[fbound>>8][fbound&255].next != M_UNIT);
}

void ListBox::AddLast(LKEY key, int n)
{
    if (fbound == dbn*256)
    {
        data[dbn] = new ListIt[256];
        for (int i=0; i<256; i++) data[dbn][i].next = M_UNIT;
        dbn++;
    }
    ListIt x;
    x.n = n;
    x.next = 0;
    LKEY end = data[key>>8][key&255].end;
    data[key>>8][key&255].end = fbound;
    data[fbound>>8][fbound&255] = x;
    data[end>>8][end&255].next = fbound;
    do fbound++;
    while (fbound < dbn*256 && data[fbound>>8][fbound&255].next != M_UNIT);
}

int ListBox::AddFirstX(LKEY key, int n)
{
    if (!Search(key, n)) { AddFirst(key, n); return 1; }
    return 0;
}

int ListBox::AddLastX(LKEY key, int n)
{
    if (!Search(key, n)) { AddLast(key, n); return 1; }
    return 0;
}

int ListBox::Delete(LKEY key, int n)
{
    LKEY prev = key;
    LKEY& end = data[key>>8][key&255].end;
    int ret = 0;
    key = data[key>>8][key&255].next;
    while (1)
    {
        if (data[key>>8][key&255].n == n)
        {
            data[prev>>8][prev&255].next = data[key>>8][key&255].next;
            data[key>>8][key&255].next = M_UNIT;
            if (fbound > key) fbound = key;
            ret++;
            if (key == end) { end = prev; return ret; }
            key = data[prev>>8][prev&255].next;
        }
        else
        {
            prev = key;
            key = data[key>>8][key&255].next;
            if (!key) return ret;
        }
    }
}

int ListBox::DeleteFirst(LKEY key)
{
    LKEY nxt = data[key>>8][key&255].next;
    if (!nxt) return 1;
    data[key>>8][key&255].next = data[nxt>>8][nxt&255].next;
    data[nxt>>8][nxt&255].next = M_UNIT;
    if (fbound > nxt) fbound = nxt;
    if (data[key>>8][key&255].end == nxt) data[key>>8][key&255].end = key;
    return 0;
}

int ListBox::Search(LKEY key, int n)
{
    int ret = 0;
    while (1)
    {
        key = data[key>>8][key&255].next;
        if (!key) return ret;
        if (data[key>>8][key&255].n == n) ret++;
    }
}

void ListBox::Insert(LKEY key, int prev_num, int n)
{
    LKEY k = key;
    for (int i=0; i<prev_num; i++) k = data[k>>8][k&255].next;
    if (!data[k>>8][k&255].next) { AddLast(key, n); return; }
    if (fbound == dbn*256)
    {
        data[dbn] = new ListIt[256];
        for (int i=0; i<256; i++) data[dbn][i].next = M_UNIT;
        dbn++;
    }
    ListIt x;
    x.n = n;
    x.next = data[k>>8][k&255].next;
    data[k>>8][k&255].next = fbound;
    data[fbound>>8][fbound&255] = x;
    do fbound++;
    while (fbound < dbn*256 && data[fbound>>8][fbound&255].next != M_UNIT);
}

void ListBox::Change(LKEY key, int num, int n)
{
    for (int i=0; i<num; i++) key = data[key>>8][key&255].next;
    data[key>>8][key&255].n = n;
}

void ListBox::DeleteByNum(LKEY key, int num)
{
    LKEY prev;
    LKEY& end = data[key>>8][key&255].end;
    while (num)
    {
        prev = key;
        key = data[key>>8][key&255].next;
        num--;
    }
    data[prev>>8][prev&255].next = data[key>>8][key&255].next;
    data[key>>8][key&255].next = M_UNIT;
    if (fbound > key) fbound = key;
    if (key == end) end = prev;
}

int ListBox::GetLast(LKEY key)
{
    LKEY k = data[key>>8][key&255].end;
    return data[k>>8][k&255].n;
}

//------------------- StringBox ---------------

StringBox::StringBox()
{
    arr_num = 1;
    arr[0] = new char[SB_LEN];
    for (int i=0; i<SB_LEN; i++) arr[0][i] = 0;
    cur_arr = 0;
    cur_adr = 0;

    d_adr[0] = new DAdr[256];
    d_adr_num = 1;
    for (i=0; i<256; i++) d_adr[0][i].a_n = M_UNIT;
    ek = 0;
    null = 0;
}

StringBox::~StringBox()
{
    for (unsigned int i=0; i<arr_num; i++) delete[](arr[i]);
    for (i=0; i<d_adr_num; i++) delete[](d_adr[i]);
}

char* StringBox::FindPlaceInCurrenArray(int len)
{
    char* s = arr[cur_arr];

 nnn:
    // Check if the current address can be used
    if (cur_adr + len > SB_LEN) return 0;
    char* s1 = s + cur_adr;
    int n = 0;

    while (n<len)
    {
        if (*s1)
        {
            cur_adr += n + 1;
            while (*s1++) cur_adr++;
            goto nnn;
        }
        s1++;
        n++;
    }

    return s + cur_adr;
}

SKEY StringBox::Put(const char* str)
{
    if (!*str) return (SKEY)(-2);

    int try_first = 1;
    SKEY key = ek;

    int len = 0;
    const char* s = str;
    while (*s++) len++;

    char* s1;
    do
    {
        s1 = FindPlaceInCurrenArray(len + 1);

        if (s1) while (*str) *s1++ = *str++;
        else
        {
            cur_arr++;
            if (cur_arr == arr_num)
                if (try_first)
                {
                    cur_arr = 0;
                    try_first = 0;
                }
                else
                {
                    arr[arr_num] = new char[SB_LEN];
                    for (int i=0; i<SB_LEN; i++) arr[arr_num][i] = 0;
                    arr_num++;
                }
            cur_adr = 0;
        }
    }
    while (!s1);

    d_adr[key>>8][key&255].a_n = cur_arr;
    d_adr[key>>8][key&255].adr = cur_adr;
    cur_adr += len + 1;

    while (d_adr[ek>>8][ek&255].a_n != M_UNIT)
    {
        ek++;

        // If empty key is outside the border
        if (ek == 256 * d_adr_num)
        {
            d_adr[d_adr_num] = new DAdr[256];
            for (int i=0; i<256; i++) d_adr[d_adr_num][i].a_n = M_UNIT;
            d_adr_num++;
        }
    }

    return key;
}

void StringBox::Delete(SKEY key)
{
    if (key == (SKEY)(-2)) return;
    unsigned int a_n = d_adr[key>>8][key&255].a_n;
    unsigned int adr = d_adr[key>>8][key&255].adr;
    if (cur_arr > a_n)
    {
        cur_arr = a_n;
        cur_adr = adr;
    }
    else if (cur_adr > adr && cur_arr == a_n) cur_adr = adr;

    d_adr[key>>8][key&255].a_n = M_UNIT;
    while (arr[a_n][adr] != 0) arr[a_n][adr++] = 0;
    if (ek > key) ek = key;
}

char* StringBox::Get(SKEY key)
{
    if (key == (SKEY)(-2)) return &null;
    return arr[d_adr[key>>8][key&255].a_n] + d_adr[key>>8][key&255].adr;
}

//----------------
// Other functions
//----------------

int StrEq(const char* s1, const char* s2)
{
    do if (*s1 != *s2++) return 0; while (*s1++);
    return 1;
}

void StrCopy(const char* sfr, char* sto)
{
    do *sto = *sfr++; while (*sto++);
}

void SetHiFrBit2(int fr)
{
    int i;
    for (i=0; i<Fragments->num; i++) fragment(i)->flags &= -3;
    fragment(fr)->flags |= 2;
    int level = fragment(fr)->level;
    for (i=0; i<Fragments->num; i++)
    {
        LKEY br = GenLists->NewList();
        int j = i;
        while (fragment(j)->level > level && (!(fragment(j)->flags&2)))
        {
            GenLists->AddFirst(br, j);
            j = fragment(j)->fr;
        }
        if (fragment(j)->flags&2)
        {
            GenLists->Get(br);
            while (GenLists->NoEnd())
                fragment(GenLists->Next())->flags |= 2;
        }
        GenLists->DestList(br);
    }
}

void ChangeVertexTN(int tp, int oldtn, int newtn)
{
    for (int i=0; i<Vertices->num; i++)
        if (vertex(i)->type == tp && vertex(i)->tn == oldtn)
        {
            vertex(i)->tn = newtn;
            return;
        }
}

void ChangeFragmentTN(int tp, int oldtn, int newtn)
{
    for (int i=0; i<Fragments->num; i++)
        if (fragment(i)->type == tp && fragment(i)->tn == oldtn)
        {
            fragment(i)->tn = newtn;
            return;
        }
}

void ChangeEdgeTN(int tp, int oldtn, int newtn)
{
    for (int i=0; i<Edges->num; i++)
        if (edge(i)->type == tp && edge(i)->tn == oldtn)
        {
            edge(i)->tn = newtn;
            return;
        }
}

void SetLevels(int fr, int lev)
{
    fragment(fr)->level = lev;
    for (int i=0; i<Fragments->num; i++)
        if (fragment(i)->fr == fr) SetLevels(i, lev + 1);
}

int StrEqF(char* s1, char* s2)
{
    while (*s1 && *s1 == *s2++) s1++;
    if (*s1) return 0; else return 1;
}

//-------------------------

// Type functions

int Type::AddTObject()
{
    for (int n=0; n<Labels->num; n++)
        switch (label(n)->data_type)
        {
        case 2:
        case 1:
            label(n)->i_val->Add(label(n)->i_defval);
            break;
        case 3:
            label(n)->f_val->Add(label(n)->f_defval);
            break;
        case 4:
            label(n)->i_val->Add(label(n)->text->Put(GenStrings->Get(label(n)->t_defval)));
            break;
        }
    return num++;
}

int Type::DeleteTObject(int m)
{
    for (int n=0; n<Labels->num; n++)
        switch (label(n)->data_type)
        {
        case 2:
        case 1:
            label(n)->i_val->Delete(m);
            break;
        case 3:
            label(n)->f_val->Delete(m);
            break;
        case 4:
            label(n)->text->Delete(*(label(n)->i_val->Acc(m)));
            label(n)->i_val->Delete(m);
            break;
        }
    return --num;
}

void Type::AddLabel(int t)
{
    Label L;
    L.name = GenStrings->Put("New");
    L.data_type = t;
    L.par = 1;
    L.width = 10;
    L.prec = 3;
    L.i_defval = 0;
    L.f_defval = .0;
    int i;
    switch (t)
    {
    case 2:
    case 1:
        L.i_val = new DinArr<int>;
        for(i=0; i<num; i++) L.i_val->Add(0);
        break;
    case 3:
        L.f_val = new DinArr<double>;
        for(i=0; i<num; i++) L.f_val->Add(0);
        break;
    case 4:
        L.i_val = new DinArr<int>;
        L.text = new StringBox;
        L.t_defval = GenStrings->Put("");
        for (i=0; i<num; i++) L.i_val->Add(L.text->Put(""));
        break;
    }
    Labels->Add(L);
}

void Type::DeleteLabel(int ln)
{
    switch (label(ln)->data_type)
    {
    case 2:
    case 1:
        delete(label(ln)->i_val);
        break;
    case 3:
        delete(label(ln)->f_val);
        break;
    case 4:
        delete(label(ln)->text);
        delete(label(ln)->i_val);
        GenStrings->Delete(label(ln)->t_defval);
        break;
    }
    GenStrings->Delete(label(ln)->name);
    int old = Labels->Delete(ln);
    LKEY nk = GenLists->NewList();

    int n;
    GenLists->Get(vis_cd);
    while (GenLists->NoEnd())
        switch (GenLists->Next())
        {
        case 1:
            n = GenLists->Next();
            if (n != ln)
            {
                GenLists->AddLast(nk, 1);
                if (n != old) GenLists->AddLast(nk, n);
                else GenLists->AddLast(nk, ln);
            }
            break;
        case 0:
            n = GenLists->Next();
            GenLists->AddLast(nk, 0);
            GenLists->AddLast(nk, n);
            break;
        case 2:
            GenLists->AddLast(nk, 2);
            break;
        }
    GenLists->DestList(vis_cd);
    vis_cd = nk;

    nk = GenLists->NewList();
    GenLists->Get(vis_cdx);
    while (GenLists->NoEnd())
        switch (GenLists->Next())
        {
        case 1:
            n = GenLists->Next();
            if (n != ln)
            {
                GenLists->AddLast(nk, 1);
                if (n != old) GenLists->AddLast(nk, n);
                else GenLists->AddLast(nk, ln);
            }
            break;
        case 0:
            n = GenLists->Next();
            GenLists->AddLast(nk, 0);
            GenLists->AddLast(nk, n);
            break;
        case 2:
            GenLists->AddLast(nk, 2);
            break;
        }
    GenLists->DestList(vis_cdx);
    vis_cdx = nk;
}

int Type::SetVisCode(char* str, int is_x)
{
    DestVisCode(is_x);
    if (is_x) vis_cdx = GenLists->NewList();
    else vis_cd = GenLists->NewList();
    LKEY vis;
    if (is_x) vis = vis_cdx;
    else vis = vis_cd;

    char b[1024];
    char* s = b;
    int st = 0;

    while (*str)
    {
        char t = *str++;
        switch (st)
        {
        case 0:
            if (t == '\\') st = 1;
            else *s++ = t;
            break;

        case 1:
            switch (t)
            {
            case '\\':
                st = 0;
                *s++ = t;
                break;
            case '{':
                st = 2;
                if (s != b)
                {
                    *s = 0;
                    s = b;
                    GenLists->AddLast(vis, 0);
                    GenLists->AddLast(vis, GenStrings->Put(b));
                }
                break;
            case 'n':
                if (s != b)
                {
                    *s = 0; s = b;
                    GenLists->AddLast(vis, 0);
                    GenLists->AddLast(vis, GenStrings->Put(b));
                }
                GenLists->AddLast(vis, 2);
                st = 0;
                break;
            default:
                return 1;
            }
            break;

        case 2:
            if (t != '}') *s++ = t;
            else
            {
                int i = 0, k = -1; *s = 0;
                while (i < Labels->num && k == -1)
                {
                    char* sss = GenStrings->Get(label(i)->name);
                    if (StrEq(sss, b)) k = i;
                    i++;
                }
                if (k == -1) return 2;
                GenLists->AddLast(vis, 1);
                GenLists->AddLast(vis, k);
                st = 0;
                s = b;
            }
            break;
        }
    }

    if (st == 2) return 3;
    if (s != b)
    {
        *s = 0;
        GenLists->AddLast(vis, 0);
        GenLists->AddLast(vis, GenStrings->Put(b));
    }
    if (st == 1) return 1;
    return 0;
}

void Type::GetVisCode(char* str, int is_x)
{
    if (is_x) GenLists->Get(vis_cdx);
    else GenLists->Get(vis_cd);
    char* s1;
    while (GenLists->NoEnd())
    {
        switch (GenLists->Next())
        {
        case 1:
            *str++ = '\\';
            *str++ = '{';
            StrCopy(GenStrings->Get(label(GenLists->Next())->name), str);
            while (*str) str++;
            *str++ = '}';
            break;

        case 0:
            s1 = GenStrings->Get(GenLists->Next());
            while (*s1)
            {
                char t = *s1++;
                *str++ = t;
                if (t == '\\') *str++ = t;
            }
            break;

        case 2:
            *str++ = '\\';
            *str++ = 'n';
            break;
        }
    }
    *str = 0;
}

void Type::DestVisCode(int is_x)
{
    if (is_x) GenLists->Get(vis_cdx);
    else GenLists->Get(vis_cd);

    while (GenLists->NoEnd())
    {
        int n = GenLists->Next();
        if (n == 1) GenLists->Next();
        if (n == 0) GenStrings->Delete(GenLists->Next());
    }

    if (is_x) GenLists->DestList(vis_cdx);
    else GenLists->DestList(vis_cd);
}

// End of 'type' functions --------------------------

// Labels functions:

void Label::GetValString(int n, char* str)
{
    switch (data_type)
    {
    case 2:
    case 1:
        if (par&1) sprintf(str,"%i", *(i_val->Acc(n)));
        else sprintf(str, "% *i", width, *(i_val->Acc(n)));
        break;
    case 3:
        if (par&1) sprintf(str,"%.*f", prec, *(f_val->Acc(n)));
        else sprintf(str, "% *.*f", width, prec, *(f_val->Acc(n)));
        break;
    case 4:
        StrCopy(text->Get(*(i_val->Acc(n))), str);
        break;
    }
}

void Label::GetDefValString(char* str)
{
    switch (data_type)
    {
    case 2:
    case 1:
        if (par&1) sprintf(str,"%i", i_defval);
        else sprintf(str, "% *i", width, i_defval);
        break;
    case 3:
        if (par&1) sprintf(str,"%.*f", prec, f_defval);
        else sprintf(str, "% *.*f", width, prec, f_defval);
        break;
    case 4:
        StrCopy(GenStrings->Get(t_defval), str);
        break;
    }
}

int Label::SetVal(int n, char* str)
{
    switch (data_type)
    {
    case 2:
    case 1:
        *(i_val->Acc(n)) = atoi(str);
        break;
    case 3:
        *(f_val->Acc(n)) = atof(str);
        break;
    case 4:
        text->Delete(*(i_val->Acc(n)));
        *(i_val->Acc(n))= text->Put(str);
        break;
    }
    return 0;
}

int Label::SetDefVal(char* str)
{
    switch (data_type)
    {
    case 2:
    case 1:
        i_defval = atoi(str);
        break;
    case 3:
        f_defval = atof(str);
        break;
    case 4:
        GenStrings->Delete(t_defval);
        t_defval = GenStrings->Put(str);
        break;
    }
    return 0;
}

void Label::SetValFromDef(int n)
{
    switch (data_type)
    {
    case 2:
    case 1:
        *(i_val->Acc(n)) = i_defval;
        break;
    case 3:
        *(f_val->Acc(n)) = f_defval;
        break;
    case 4:
        text->Delete(*(i_val->Acc(n)));
        *(i_val->Acc(n))= text->Put(GenStrings->Get(t_defval));
        break;
    }
}

// End of 'labels' functions ---------------------------

// General functions

void AddVertex(int t, int fr, int x, int y)
{
    Vertex n;
    n.type = t;
    n.x = x;
    n.y = y;
    n.w = type[t].defwidth;
    n.h = type[t].defheight;
    n.fr = fr;
    n.tn = type[t].AddTObject();
    n.flags = 0;
    n.in = InEdges->NewList();
    n.out = OutEdges->NewList();
    n.dx = 10;
    n.dy = 0;
    Vertices->Add(n);
}

void AddEdge(int t, int from, int to)
{
    // Add an edge
    Edge e;
    e.type = t;
    e.from = from;
    e.to = to;
    e.tn = type[t].AddTObject();
    e.dx1 = 0; e.dy1 = 0; e.dx2 = 0; e.dy2 = 0;
    e.ort = type[t].def_ort;
    e.xbends = XBends->NewList();
    e.ybends = YBends->NewList();

    switch (type[t].shape)
    {
    case 0:
    case 2:
        if (type[t].lv_par & 1) e.lpt = 12;
        else
            if (((long)(vertex(from)->x - vertex(to)->x))*((long)(vertex(from)->y - vertex(to)->y)) > 0) e.lpt = 9;
            else e.lpt = 8;
            break;
    }

    int n = Edges->Add(e);
    OutEdges->AddFirst(vertex(from)->out, n);
    InEdges->AddFirst(vertex(to)->in, n);
}

void DeleteEdge(int n)
{
    OutEdges->Delete(vertex(edge(n)->from)->out, n);
    InEdges->Delete(vertex(edge(n)->to)->in, n);
    int m = type[edge(n)->type].DeleteTObject(edge(n)->tn);
    ChangeEdgeTN(edge(n)->type, m, edge(n)->tn);
    m = Edges->Delete(n);
    if (m != n)
    {
        OutEdges->Delete(vertex(edge(n)->from)->out, m);
        OutEdges->AddFirst(vertex(edge(n)->from)->out, n);
        InEdges->Delete(vertex(edge(n)->to)->in, m);
        InEdges->AddFirst(vertex(edge(n)->to)->in, n);
    }
}

void DeleteVertex(int n)
{
    int m = type[vertex(n)->type].DeleteTObject(vertex(n)->tn);
    ChangeVertexTN(vertex(n)->type, m, vertex(n)->tn);

    while (InEdges->Get(vertex(n)->in))
        DeleteEdge(InEdges->Next());
    while (OutEdges->Get(vertex(n)->out))
        DeleteEdge(OutEdges->Next());
    InEdges->DestList(vertex(n)->in);
    OutEdges->DestList(vertex(n)->out);

    Vertices->Delete(n);
    if (Vertices->num != n)
    {
        OutEdges->Get(vertex(n)->out);
        while (OutEdges->NoEnd())
            edge(OutEdges->Next())->from = n;
        InEdges->Get(vertex(n)->in);
        while (InEdges->NoEnd())
            edge(InEdges->Next())->to = n;
    }
}

void CreateFragment(int t, int fr, int x, int y, int w, int h)
{
    Fragment F;
    F.type = t;
    F.x = x; F.y = y;
    F.w = w;
    F.h = h;
    F.scroll_x = F.scroll_y = 0;
    F.scale = fragment(fr)->scale;
    F.fr = fr;
    F.tn = type[t].AddTObject();
    F.kind = 1;
    F.level = fragment(fr)->level+1;
    F.title = GenStrings->Put("New fragment");
    F.flags = 0;
    F.dx = -10;
    F.dy = -10;
    F.WinRect.left = 100;
    F.WinRect.top = 100;
    F.WinRect.right = 260;
    F.WinRect.bottom = 260;
    F.ID = Next_F_ID++;
    F.ptw = NULL;
    Fragments->Add(F);
    SetLevels(0, 0);
}

 void Unfold(int fr)
{
     int m = type[fragment(fr)->type].DeleteTObject(fragment(fr)->tn);
     ChangeFragmentTN(fragment(fr)->type, m, fragment(fr)->tn);

     int frn = fragment(fr)->fr;
     int i;
     for (i=0; i<Vertices->num; i++)
         if (vertex(i)->fr == fr) vertex(i)->fr = frn;
     for (i=0; i<Fragments->num; i++)
         if (fragment(i)->fr == fr) fragment(i)->fr = frn;

     int old = Fragments->Delete(fr);

     for (i=0; i<Vertices->num; i++)
         if (vertex(i)->fr == old) vertex(i)->fr = fr;
     for (i=0; i<Fragments->num; i++)
         if (fragment(i)->fr == old) fragment(i)->fr = fr;
     SetLevels(frn, fragment(frn)->level);
}

void MoveVertexToFragment(int v, int fr)
{
    vertex(v)->fr = fr;
}

void OptimizeFrLoc(int fr, int BB_Dist)
{
    int x = fragment(fr)->x;
    int y = fragment(fr)->y;
    int x1 = x + fragment(fr)->w;
    int y1 = y + fragment(fr)->h;
    int ft = 1;

    int i;
    for (i=0; i<Fragments->num; i++)
        if (fragment(i)->fr == fr)
        {
            if (ft)
            {
                x = x1 = fragment(i)->x;
                y = y1 = fragment(i)->y;
                ft = 0;
            }
            if (x > fragment(i)->x - BB_Dist) x = fragment(i)->x - BB_Dist;
            if (y > fragment(i)->y - BB_Dist) y = fragment(i)->y - BB_Dist;
            if (x1 < fragment(i)->x + fragment(i)->w + BB_Dist)
                x1 = fragment(i)->x + fragment(i)->w + BB_Dist;
            if (y1 < fragment(i)->y + fragment(i)->h + BB_Dist)
                y1 = fragment(i)->y + fragment(i)->h + BB_Dist;
        }

    for (i=0; i<Vertices->num; i++)
        if (vertex(i)->fr == fr)
        {
            if (ft)
            {
                x = x1 = vertex(i)->x;
                y = y1 = vertex(i)->y;
                ft = 0;
            }
            if (x > vertex(i)->x - vertex(i)->w - BB_Dist)
                x = vertex(i)->x - vertex(i)->w - BB_Dist;
            if (y > vertex(i)->y - vertex(i)->h - BB_Dist)
                y = vertex(i)->y - vertex(i)->h - BB_Dist;
            if (x1 < vertex(i)->x + vertex(i)->w + BB_Dist)
                x1 = vertex(i)->x + vertex(i)->w + BB_Dist;
            if (y1 < vertex(i)->y + vertex(i)->h + BB_Dist)
                y1 = vertex(i)->y + vertex(i)->h + BB_Dist;
        }

    fragment(fr)->x = x;
    fragment(fr)->y = y;
    fragment(fr)->w = x1 - x;
    fragment(fr)->h = y1 - y;
}

int AddType(int object, char* name)
{
    for (int i=0; i<typ_num; i++)
        if (StrEq(GenStrings->Get(type[i].name), name)) return 0;
    Type t;
    t.object = object;
    t.b_style = 0;
    t.defwidth = t.defheight = 20;
    t.num = 0;
    t.def_ort = 0;
    t.l_col = 0;
    t.l_colx = 0;

    switch (object)
    {
    case 0:
        t.lv_par = 0x5A | 0x80 ;
        t.b_style = 0;
        t.shape = 0;
        t.b_color = RGB(0, 0, 192);
        t.i_color = RGB(224, 224, 224);
        t.Lab_Dist = 2;
        break;

    case 1:
        t.lv_par = 108;
        t.b_color = RGB(224, 224, 192);
        t.i_color = RGB(224, 192, 224);
        t.Lab_Dist = 1;
        break;

    case 2:
        t.lv_par = 91;
        t.shape = 2;
        t.b_color = RGB(0, 0, 0);
        t.Lab_Dist = 2;
        break;
    }

    t.name = GenStrings->Put(name);
    t.Labels = new DinArr<Label>;
    t.lf = DefLogFont;
    t.lfx = DefLogFont2;
    t.vis_cd = GenLists->NewList();
    t.vis_cdx = GenLists->NewList();
    t.Arrow_wid = 24;
    t.Arrow_len = 20;
    t.ArrSScale = 1;
    t.ArrShape = 2;
    type[typ_num] = t;
    return typ_num++;
}

int DeleteType(int tn, int par)
{
    int obj = type[tn].object;
    int i = 0;
    if (!par)
    {
        while (i < typ_num && (type[i].object != obj || i == tn)) i++;
        if (i == typ_num) return obj+1;
        if (Ex && type[tn].num) return 4;
    }

    while (type[tn].Labels->num) type[tn].DeleteLabel(0);

    type[tn].DestVisCode(0);
    type[tn].DestVisCode(1);

    delete(type[tn].Labels);

    GenStrings->Delete(type[tn].name);
    typ_num--;
    type[tn] = type[typ_num];

    int j;
    if (Ex)
        switch (obj)
        {
        case 0:
            for (j=0; j<Vertices->num; j++)
                if (vertex(j)->type == typ_num) vertex(j)->type = tn;
            break;

        case 1:
            for (j=0; j<Fragments->num; j++)
                if (fragment(j)->type == typ_num) fragment(j)->type = tn;
            break;

        case 2:
            for (j=0; j<Edges->num; j++)
                if (edge(j)->type == typ_num) edge(j)->type = tn;
            break;
        }
    return 0;
}

void CreateNewGraph(int t)
{
    if (Ex) DestroyGraph();
    InEdges = new ListBox;
    OutEdges = new ListBox;
    Vertices = new DinArr<Vertex>;
    Fragments = new DinArr<Fragment>;
    Edges = new DinArr<Edge>;
    XBends = new ListBox;
    YBends = new ListBox;

    Fragment F;
    F.type = t;
    F.x = F.y = 0;
    F.scroll_x = F.scroll_y = 0;
    int w = 640, h = 480;
    F.WinRect.left = 16;
    F.WinRect.top = 16;
    F.WinRect.right = w - 120;
    F.WinRect.bottom = h - 72;
    F.w = w - 136;
    F.h = h - 90;
    F.scale = 100;
    F.fr = -1;
    F.tn = 0;
    F.kind = 0;
    F.level = 0;
    F.flags = 0;
    F.title = GenStrings->Put("New Graph");
    F.ID = 0;
    F.dx = -10;
    F.dy = -10;
    Next_F_ID = 1;
    F.ptw = NULL;
    type[t].AddTObject();
    Fragments->Add(F);
    Ex = 1;
}

void DestroyGraph()
{
    if (!Ex) return;
    int i;
    for (i=0; i<Fragments->num; i++)
        GenStrings->Delete(fragment(i)->title);
    delete(Vertices);
    delete(Fragments);
    delete(Edges);
    delete(InEdges);
    delete(OutEdges);
    delete(XBends);
    delete(YBends);
    for (i=0; i<typ_num; i++)
        while (type[i].num) type[i].DeleteTObject(0);
    Ex = 0;
}

void BeginWork()
{
    type = new Type[256];
    GenLists = new ListBox;
    EUStack = new Stack;
    if (!GenStrings) GenStrings = new StringBox;
    DefLogFont.lfHeight = 29;
    StrCopy("Arial", DefLogFont.lfFaceName);
    DefLogFont2.lfHeight = 29;
    StrCopy("Arial", DefLogFont2.lfFaceName);
    AddType(0, "Default vertex");
    AddType(1, "Default fragment");
    AddType(2, "Default edge");

    // Find sizes of basic types (for saving)
    Vertex V;
    Fragment F;
    Edge E;
    Type T;
    Label L;
    Vertex_Save_Size = ((char*)&(V.flags)) - (char*)(&V);
    Fragment_Save_Size = ((char*)&(F.flags)) - (char*)(&F);
    Edge_Save_Size = ((char*)&(E.flags)) - (char*)(&E);
    Type_Save_Size = ((char*)&(T.vis_cd)) - (char*)(&T);
    Label_Save_Size = ((char*)&(L.t_defval)) - (char*)(&L);
}

void EndWork()
{
    if (Ex) DestroyGraph();
    while (typ_num) DeleteType(0, 1);
    delete(GenLists);
    delete(EUStack);
    delete(GenStrings);
    delete[](type);
}

void SaveString(int hd, char* str)
{
    char* s = str; int i = 1;
    while (*s++) i++;
    _write(hd, str, i);
}

void SaveList(int hd, ListBox* LB, LKEY key)
{
    int i = 0;
    LB->Get(key);
    while (LB->NoEnd()) { LB->Next(); i++; }
    _write(hd, &i, 4);
    LB->Get(key);
    while (LB->NoEnd())
    {
        i = LB->Next();
        _write(hd, &i, 4);
    }
}

int SaveGraph(char* filename)
{
    int fhd = open(filename, O_WRONLY | O_CREAT | O_BINARY | O_TRUNC, S_IWRITE);
    if (fhd == -1) return 1;

    PushOutEdge();
    PushInEdge();

    SaveString(fhd, "HIGRES 3.XX Graph File.");
    int version = 0;
    _write(fhd, &version, 4);

    _write(fhd, &Type_Save_Size, 4);
    _write(fhd, &Vertex_Save_Size, 4);
    _write(fhd, &Fragment_Save_Size, 4);
    _write(fhd, &Edge_Save_Size, 4);
    _write(fhd, &Label_Save_Size, 4);

    // Save types
    _write(fhd, &typ_num, 4);
    for (int i=0; i<typ_num; i++)
    {
        _write(fhd, &(type[i]), Type_Save_Size);
        SaveString(fhd, GenStrings->Get(type[i].name));

        // Save vis. code
        int j = 0;
        GenLists->Get(type[i].vis_cd);
        while (GenLists->NoEnd())
        {
            j = GenLists->Next();
            _write(fhd, &j, 4);
            switch (j)
            {
            case 0:
                SaveString(fhd, GenStrings->Get(GenLists->Next()));
                break;
            case 1:
                int n = GenLists->Next();
                _write(fhd, &n, 4);
                break;
            }
        }
        j = -1;
        _write(fhd, &j, 4);

        // Save ext. vis. code
        GenLists->Get(type[i].vis_cdx);
        while (GenLists->NoEnd())
        {
            j = GenLists->Next();
            _write(fhd, &j, 4);
            switch (j)
            {
            case 0:
                SaveString(fhd, GenStrings->Get(GenLists->Next()));
                break;
            case 1:
                int n = GenLists->Next();
                _write(fhd, &n, 4);
                break;
            }
        }
        j = -1;
        _write(fhd, &j, 4);

        j = type[i].Labels->num;
        _write(fhd, &j, 4);
        for (j=0; j<type[i].Labels->num; j++)
        {
            _write(fhd, type[i].Labels->Acc(j), Label_Save_Size);
            SaveString(fhd, GenStrings->Get(type[i].Labels->Acc(j)->name));
            int k;
            switch (type[i].Labels->Acc(j)->data_type)
            {
            case 2:
            case 1:
                for (k=0; k<type[i].num; k++)
                    _write(fhd, type[i].Labels->Acc(j)->i_val->Acc(k), 4);
                break;
            case 3:
                for (k=0; k<type[i].num; k++)
                    _write(fhd, type[i].Labels->Acc(j)->f_val->Acc(k), 8);
                break;
            case 4:
                SaveString(fhd, GenStrings->Get(type[i].Labels->Acc(j)->t_defval));
                for (k=0; k<type[i].num; k++)
                    SaveString(fhd, type[i].Labels->Acc(j)->text->Get(*(type[i].Labels->Acc(j)->i_val->Acc(k))));
                break;
            }
        }
    }

    // Save vertices
    i = Vertices->num;
    _write(fhd, &i, 4);
    for (i=0; i<Vertices->num; i++)
    {
        _write(fhd, vertex(i), Vertex_Save_Size);
        SaveList(fhd, InEdges, vertex(i)->in);
        SaveList(fhd, OutEdges, vertex(i)->out);
    }

    // Save fragments
    i = Fragments->num;
    _write(fhd, &i, 4);
    for (i=0; i<Fragments->num; i++)
    {
        _write(fhd, fragment(i), Fragment_Save_Size);
        SaveString(fhd, GenStrings->Get(fragment(i)->title));
    }

    // Save edges
    i = Edges->num;
    _write(fhd, &i, 4);
    for (i=0; i<Edges->num; i++)
    {
        _write(fhd, edge(i), Edge_Save_Size);
        SaveList(fhd, XBends, edge(i)->xbends);
        SaveList(fhd, YBends, edge(i)->ybends);
    }

    // Save other parameters

    int dvt = 0;
    while (type[dvt].object != 0) dvt++;
    int det = 0;
    while (type[det].object != 2) det++;
    int dft = 0;
    while (type[dft].object != 1) dft++;

    _write(fhd, &dvt, 4);
    _write(fhd, &dft, 4);
    _write(fhd, &det, 4);

    _write(fhd, &RRPar, 4);
    _write(fhd, &GGenOpt, 4);
    _write(fhd, &GrType, 4);
    _write(fhd, &Grid_x, 4);
    _write(fhd, &Grid_y, 4);
    _write(fhd, &Grid_w, 4);
    _write(fhd, &Grid_h, 4);

    close(fhd);

    PopOutEdge();
    PopInEdge();

    return 0;
}

void LoadString(int hd, char* str, int len = 0)
{
    do
    {
        _read(hd, str, 1);
        len--;
    }
    while (*str++ && len);
}

void LoadList(int hd, ListBox* LB, LKEY& key)
{
    int n, k;
    _read(hd, &n, 4);
    key = LB->NewList();
    for (int i=0; i<n; i++)
    {
        _read(hd, &k, 4);
        LB->AddLast(key, k);
    }
}

int LoadGraph(char* filename)
{ 
    int fhd = open(filename, O_RDONLY | O_BINARY);
    if (fhd == -1) return 1;
    char str[1024];
    LoadString(fhd, str, 64);
    if (!StrEq(str, "HIGRES 3.XX Graph File."))
    {
        close(fhd);
        return 2;
    }

    // Read version
    int ver;
    _read(fhd, &ver, 4);

    // Destroy current graph and create new structures
    DestroyGraph();
    while (typ_num) DeleteType(0, 1);
    InEdges = new ListBox;
    OutEdges = new ListBox;
    Vertices = new DinArr<Vertex>;
    Fragments = new DinArr<Fragment>;
    Edges = new DinArr<Edge>;
    XBends = new ListBox;
    YBends = new ListBox;
    Next_F_ID = 0;

    int type_size;
    int vertex_size;
    int fr_size;
    int edge_size;
    int label_size;

    _read(fhd, &type_size, 4);
    _read(fhd, &vertex_size, 4);
    _read(fhd, &fr_size, 4);
    _read(fhd, &edge_size, 4);
    _read(fhd, &label_size, 4);

    // Read types
    _read(fhd, &typ_num, 4);
    for (int i=0; i<typ_num; i++)
    {
        _read(fhd, &(type[i]), type_size);
        LoadString(fhd, str);
        type[i].name = GenStrings->Put(str);
        type[i].Labels = new DinArr<Label>;
        type[i].vis_cd = GenLists->NewList();
        type[i].vis_cdx = GenLists->NewList();

        int j, n, m;
        do
        {
            _read(fhd, &m, 4);
            if (m != -1) GenLists->AddLast(type[i].vis_cd, m);
            switch (m)
            {
            case 0:
                LoadString(fhd, str);
                GenLists->AddLast(type[i].vis_cd, GenStrings->Put(str));
                break;
            case 1:
                _read(fhd, &n, 4);
                GenLists->AddLast(type[i].vis_cd, n);
                break;
            }
        }
        while (m != -1);

        do
        {
            _read(fhd, &m, 4);
            if (m != -1) GenLists->AddLast(type[i].vis_cdx, m);
            switch (m)
            {
            case 0:
                LoadString(fhd, str);
                GenLists->AddLast(type[i].vis_cdx, GenStrings->Put(str));
                break;
            case 1:
                _read(fhd, &n, 4);
                GenLists->AddLast(type[i].vis_cdx, n);
                break;
            }
        }
        while (m != -1);

        _read(fhd, &n, 4);
        for (j=0; j<n; j++)
        {
            Label L;
            _read(fhd, &L, label_size);
            LoadString(fhd, str);
            L.name = GenStrings->Put(str);
            int k;
            switch (L.data_type)
            {
            case 2:
            case 1:
                L.i_val = new DinArr<int>;
                for (k=0; k<type[i].num; k++)
                {
                    int a;
                    _read(fhd, &a, 4);
                    L.i_val->Add(a);
                }
                break;
            case 3:
                L.f_val = new DinArr<double>;
                for (k=0; k<type[i].num; k++)
                {
                    double a;
                    _read(fhd, &a, 8);
                    L.f_val->Add(a);
                }
                break;
            case 4:
                L.i_val = new DinArr<int>;
                L.text = new StringBox;
                LoadString(fhd, str);
                L.t_defval = GenStrings->Put(str);
                for (k=0; k<type[i].num; k++)
                {
                    LoadString(fhd, str);
                    L.i_val->Add(L.text->Put(str));
                }
                break;
            }
            type[i].Labels->Add(L);
        }
    }

    // Read vertices
    Vertex N;
    N.flags = 0;
    int k;
    _read(fhd, &k, 4);
    for (i=0; i<k; i++)
    {
        Vertices->Add(N);
        _read(fhd, vertex(i), vertex_size);
        LoadList(fhd, InEdges, vertex(i)->in);
        LoadList(fhd, OutEdges, vertex(i)->out);
    }

    // Read fragments
    Fragment F;
    F.flags = 0;
    F.ptw = NULL;
    _read(fhd, &k, 4);
    for (i=0; i<k; i++)
    {
        Fragments->Add(F);
        _read(fhd, fragment(i), fr_size);
        if (Next_F_ID <= fragment(i)->ID) Next_F_ID = fragment(i)->ID + 1;
        LoadString(fhd, str);
        fragment(i)->title = GenStrings->Put(str);
    }

    // Read edges
    Edge E;
    E.flags = 0;
    _read(fhd, &k, 4);
    for (i=0; i<k; i++)
    {
        Edges->Add(E);
        _read(fhd, edge(i), edge_size);
        LoadList(fhd, XBends, edge(i)->xbends);
        LoadList(fhd, YBends, edge(i)->ybends);
    }

    int dum;
    _read(fhd, &dum, 4);
    _read(fhd, &dum, 4);
    _read(fhd, &dum, 4);
    _read(fhd, &RRPar, 4);
    _read(fhd, &GGenOpt, 4);
    _read(fhd, &GrType, 4);
    _read(fhd, &Grid_x, 4);
    _read(fhd, &Grid_y, 4);
    _read(fhd, &Grid_w, 4);
    _read(fhd, &Grid_h, 4);

    close(fhd);
    Ex = 1;
    return 0;
}

int AddLabel_Ex(int t, int d)
{
    type[t].AddLabel(d);
    return type[t].Labels->num - 1;
}

void DeleteLabel(int t, int ln)
{
    type[t].DeleteLabel(ln);
}

int SetVisCode_Ex(int t, char* str, int is_x)
{
    return type[t].SetVisCode(str, is_x);
}

void GetVisCode(int t, char* str, int is_x)
{
    type[t].GetVisCode(str, is_x);
}

int GetLabVal(int obj, int n, int ln, char* str)
{
    int t, tn;
    switch (obj)
    {
    case 0:
        t = vertex(n)->type;
        tn = vertex(n)->tn;
        break;
    case 1:
        t = fragment(n)->type;
        tn = fragment(n)->tn;
        break;
    case 2:
        t = edge(n)->type;
        tn = edge(n)->tn;
        break;
    }
    if (type[t].Labels->num <= ln) return 1;
    type[t].Labels->Acc(ln)->GetValString(tn, str);
    return 0;
}

int GetLabDefVal(int t, int ln, char* str)
{
    if (type[t].Labels->num <= ln) return 1;
    type[t].Labels->Acc(ln)->GetDefValString(str);
    return 0;
}

int SetLabVal(int obj, int n, int ln, char* str)
{
    int t, tn;
    switch (obj)
    {
    case 0:
        t = vertex(n)->type;
        tn = vertex(n)->tn;
        break;
    case 1:
        t = fragment(n)->type;
        tn = fragment(n)->tn;
        break;
    case 2:
        t = edge(n)->type;
        tn = edge(n)->tn;
        break;
    }
    if (type[t].Labels->num <= ln) return 1;
    return type[t].Labels->Acc(ln)->SetVal(tn, str);
}

int SetLabDefVal(int t, int ln, char* str)
{
    if (type[t].Labels->num <= ln) return 1;
    return type[t].Labels->Acc(ln)->SetDefVal(str);
}

int ResetLabVal(int obj, int n, int ln)
{
    int t, tn;
    switch (obj)
    {
    case 0:
        t = vertex(n)->type;
        tn = vertex(n)->tn;
        break;
    case 1:
        t = fragment(n)->type;
        tn = fragment(n)->tn;
        break;
    case 2:
        t = edge(n)->type;
        tn = edge(n)->tn;
        break;
    }
    if (type[t].Labels->num <= ln) return 1;
    type[t].Labels->Acc(ln)->SetValFromDef(tn);
    return 0;
}

/*int SetLabVal(int obj, int n, int ln, int val)
{
    int t, tn;
    switch (obj)
    {
    case 0:
        t = vertex(n)->type;
        tn = vertex(n)->tn;
        break;
    case 1:
        t = fragment(n)->type;
        tn = fragment(n)->tn;
        break;
    case 2:
        t = edge(n)->type;
        tn = edge(n)->tn;
        break;
    }

    if (type[t].Labels->num <= ln) return 1;
    
    switch (type[t].Labels->Acc(ln)->data_type)
    {
    case 1:
    case 2:
        *(type[t].Labels->Acc(ln)->i_val->Acc(tn)) = val;
        break;
    case 3:
        *(type[t].Labels->Acc(ln)->f_val->Acc(tn)) = val;
        break;
    case 4:
        {
            char b[256];
            sprintf(b, "%d", val);
            SetLabVal(obj, n, ln, b);
        }
        break;
    }
 return 0;
}

 int SetLabVal(int obj, int n, int ln, double val)
{
    int t, tn;
    switch (obj)
    {
    case 0:
        t = vertex(n)->type;
        tn = vertex(n)->tn;
        break;
    case 1:
        t = fragment(n)->type;
        tn = fragment(n)->tn;
        break;
    case 2:
        t = edge(n)->type;
        tn = edge(n)->tn;
        break;
    }

    if (type[t].Labels->num <= ln) return 1;

    switch (type[t].Labels->Acc(ln)->data_type)
    {
    case 1:
    case 2:
        *(type[t].Labels->Acc(ln)->i_val->Acc(tn)) = (int)val;
        break;
    case 3:
        *(type[t].Labels->Acc(ln)->f_val->Acc(tn)) = val;
        break;
    case 4:
        {
            char b[256];
            sprintf(b, "%.*f", 3, val);
            SetLabVal(obj, n, ln, b);
        }
        break;
    }

    return 0;
}
*/
int GetLabVal(int obj, int n, int ln, int& val)
{
    int t, tn;
    switch (obj)
    {
    case 0:
        t = vertex(n)->type;
        tn = vertex(n)->tn;
        break;
    case 1:
        t = fragment(n)->type;
        tn = fragment(n)->tn;
        break;
    case 2:
        t = edge(n)->type;
        tn = edge(n)->tn;
        break;
    }

    if (type[t].Labels->num <= ln) return 1;

    switch (type[t].Labels->Acc(ln)->data_type)
    {
    case 1:
    case 2:
        val = *(type[t].Labels->Acc(ln)->i_val->Acc(tn));
        return 0;
    case 3:
        val = (int)(*(type[t].Labels->Acc(ln)->f_val->Acc(tn)));
        return 0;
    case 4:
        return 2;
    }
    return 0;
}

int GetLabVal(int obj, int n, int ln, double& val)
{
    int t, tn;
    switch (obj)
    {
    case 0:
        t = vertex(n)->type;
        tn = vertex(n)->tn;
        break;
    case 1:
        t = fragment(n)->type;
        tn = fragment(n)->tn;
        break;
    case 2:
        t = edge(n)->type;
        tn = edge(n)->tn;
        break;
    }

    if (type[t].Labels->num <= ln) return 1;

    switch (type[t].Labels->Acc(ln)->data_type)
    {
    case 1:
    case 2:
        val = *(type[t].Labels->Acc(ln)->i_val->Acc(tn));
        return 0;
    case 3:
        val = *(type[t].Labels->Acc(ln)->f_val->Acc(tn));
        return 0;
    case 4:
        return 2;
    }
    return 0;
}

int SetLabDefVal(int t, int ln, int val)
{
    if (type[t].Labels->num <= ln) return 1;
    switch (type[t].Labels->Acc(ln)->data_type)
    {
    case 1:
    case 2:
        type[t].Labels->Acc(ln)->i_defval = val;
        break;
    case 3:
        type[t].Labels->Acc(ln)->f_defval = val;
        break;
    case 4:
        {
            char b[256];
            sprintf(b, "%d", val);
            SetLabDefVal(t, ln, b);
        }
        break;
    }
    return 0;
}

int SetLabDefVal(int t, int ln, double val)
{
    if (type[t].Labels->num <= ln) return 1;
    switch (type[t].Labels->Acc(ln)->data_type)
    {
    case 1:
    case 2:
        type[t].Labels->Acc(ln)->i_defval = (int)val;
        break;
    case 3:
        type[t].Labels->Acc(ln)->f_defval = val;
        break;
    case 4:
        {
            char b[256];
            sprintf(b, "%.*f", 3, val);
            SetLabDefVal(t, ln, b);
        }
        break;
    }
    return 0;
}

int GetLabDefVal(int t, int ln, int& val)
{
    if (type[t].Labels->num <= ln) return 1;
    switch (type[t].Labels->Acc(ln)->data_type)
    {
    case 1:
    case 2:
        val = type[t].Labels->Acc(ln)->i_defval;
        return 0;
    case 3:
        val = (int)(type[t].Labels->Acc(ln)->f_defval);
        return 0;
    case 4:
        return 2;
    }
    return 0;
}

int GetLabDefVal(int t, int ln, double& val)
{
    if (type[t].Labels->num <= ln) return 1;
    switch (type[t].Labels->Acc(ln)->data_type)
    {
    case 1:
    case 2:
        val = type[t].Labels->Acc(ln)->i_defval;
        return 0;
    case 3:
        val = type[t].Labels->Acc(ln)->f_defval;
        return 0;
    case 4:
        return 2;
    }
    return 0;
}

void DeleteAllBends(int n)
{
    XBends->DestList(edge(n)->xbends);
    edge(n)->xbends = XBends->NewList();
    YBends->DestList(edge(n)->ybends);
    edge(n)->ybends = YBends->NewList();
}

void AddBendFirst(int n, int x, int y)
{
    XBends->AddFirst(edge(n)->xbends, x);
    YBends->AddFirst(edge(n)->ybends, y);
}

void AddBendLast(int n, int x, int y)
{
    XBends->AddLast(edge(n)->xbends, x);
    YBends->AddLast(edge(n)->ybends, y);
}

void InsertBend(int n, int bn, int x, int y)
{
    XBends->Insert(edge(n)->xbends, bn, x);
    YBends->Insert(edge(n)->ybends, bn, y);
}

void DeleteBend(int n, int bn)
{
    XBends->DeleteByNum(edge(n)->xbends, bn);
    YBends->DeleteByNum(edge(n)->ybends, bn);
}

void DeleteFirstBend(int n)
{
    XBends->DeleteFirst(edge(n)->xbends);
    YBends->DeleteFirst(edge(n)->ybends);
}

void GetOutEdges(int n)
{
    OutEdges->Get(vertex(n)->out);
}

int NextOutEdge(int& e)
{
    if (OutEdges->NoEnd())
    {
        e = OutEdges->Next();
        return 1;
    }
    else return 0;
}

void GetInEdges(int n)
{
    InEdges->Get(vertex(n)->in);
}

int NextInEdge(int& e)
{
    if (InEdges->NoEnd())
    {
        e = InEdges->Next();
        return 1;
    }
    else return 0;
}

void GetEdges(int n)
{
    EUSwitch = 0;
    GetInEdges(n);
    EUNum = n;
}

int NextEdge(int& e)
{
    if (EUSwitch) return NextOutEdge(e);
    if (NextInEdge(e)) return 1;
    GetOutEdges(EUNum);
    EUSwitch = 1;
    return NextOutEdge(e);
}

void PushOutEdge()
{
    OutEdges->PushCur();
}

void PushInEdge()
{
    InEdges->PushCur();
}

void PushEdge()
{
    if (EUSwitch) OutEdges->PushCur();
    else InEdges->PushCur();
    EUStack->Push(EUSwitch);
    EUStack->Push(EUNum);
}

void PopOutEdge()
{
    OutEdges->PopCur();
}

void PopInEdge()
{
    InEdges->PopCur();
}

void PopEdge()
{
    EUStack->Pop(EUNum);
    EUStack->Pop(EUSwitch);
    if (EUSwitch) OutEdges->PopCur();
    else InEdges->PopCur();
}

void GetBends(int n)
{
    XBends->Get(edge(n)->xbends);
    YBends->Get(edge(n)->ybends);
}

int NextBend(int& x, int& y)
{
    if (XBends->NoEnd())
    {
        x = XBends->Next();
        y = YBends->Next();
        return 1;
    }
    else return 0;
}

void SetFrTitle(int fr, char* str)
{
    GenStrings->Delete(fragment(fr)->title);
    fragment(fr)->title = GenStrings->Put(str);
}

void SetLabelName(int t, int ln, char* str)
{
    GenStrings->Delete(type[t].label(ln)->name);
    type[t].label(ln)->name = GenStrings->Put(str);
}

int SetTypeName(int t, char* str)
{
    for (int i=0; i<typ_num; i++)
        if (i != t && (StrEq(GenStrings->Get(type[i].name), str)))
            return 1;
    GenStrings->Delete(type[t].name);
    type[t].name = GenStrings->Put(str);
    return 0;
}

char* GetFrTitle(int fr)
{
    return GenStrings->Get(fragment(fr)->title);
}

char* GetLabelName(int t, int ln)
{
    return GenStrings->Get(type[t].label(ln)->name);
}

char* GetTypeName(int t)
{
    return GenStrings->Get(type[t].name);
}

int GetLabelNumber(int t, char* str)
{
    for (int i=0; i<type[t].Labels->num; i++)
        if (StrEq(GenStrings->Get(type[t].label(i)->name), str)) return i;
    return -1;
}

void ChangeVertexType(int n, int new_type)
{
    int m = type[vertex(n)->type].DeleteTObject(vertex(n)->tn);
    ChangeVertexTN(vertex(n)->type, m, vertex(n)->tn);
    vertex(n)->type = new_type;
    vertex(n)->tn = type[new_type].AddTObject();
}

void ChangeFragmentType(int n, int new_type)
{
    int m = type[fragment(n)->type].DeleteTObject(fragment(n)->tn);
    ChangeFragmentTN(fragment(n)->type, m, fragment(n)->tn);
    fragment(n)->type = new_type;
    fragment(n)->tn = type[new_type].AddTObject();
}

void ChangeEdgeType(int n, int new_type)
{
    int m = type[edge(n)->type].DeleteTObject(edge(n)->tn);
    ChangeEdgeTN(edge(n)->type, m, edge(n)->tn);
    edge(n)->type = new_type;
    edge(n)->tn = type[new_type].AddTObject();
}

void SimplifyGraph()
{
    int et = typ_num;
    AddType(2, "Very long temporary edge type name to prevent aaa... what?");
    int ft = typ_num;
    AddType(1, "Very long temporary fragment type name to prevent aaa... what?");
    int vt = typ_num;
    AddType(0, "Very long temporary vertex type name to prevent aaa... what?");
    int i;
    for (i=0; i<Vertices->num; i++) ChangeVertexType(i, vt);
    for (i=0; i<Fragments->num; i++) ChangeFragmentType(i, ft);
    for (i=0; i<Edges->num; i++) ChangeEdgeType(i, et);
    DeleteType(0);
    DeleteType(1);
    DeleteType(2);
    while (typ_num != 3) DeleteType(3);
    SetTypeName(0, "Simple vertex");
    SetTypeName(1, "Simple fragment");
    SetTypeName(2, "Simple edge");
}

void UnfoldAll()
{
    while (Fragments->num != 1) Unfold(1);
}

void RemoveAllBends(int par)
{
    for (int i=0; i<Edges->num; i++)
        if (!par || (edge(i)->from != edge(i)->to))
        {
            XBends->DestList(edge(i)->xbends);
            YBends->DestList(edge(i)->ybends);
            edge(i)->xbends = XBends->NewList();
            edge(i)->ybends = YBends->NewList();
        }
}

void RenumV(int v1, int v2)
{
    Vertex V;
    OutEdges->Get(vertex(v1)->out);
    while (OutEdges->NoEnd())
        edge(OutEdges->Next())->from = v2;

    OutEdges->Get(vertex(v2)->out);
    while (OutEdges->NoEnd())
        edge(OutEdges->Next())->from = v1;

    InEdges->Get(vertex(v1)->in);
    while (InEdges->NoEnd())
        edge(InEdges->Next())->to = v2;

    InEdges->Get(vertex(v2)->in);
    while (InEdges->NoEnd())
        edge(InEdges->Next())->to = v1;

    V = *(vertex(v1));
    *(vertex(v1)) = *(vertex(v2));
    *(vertex(v2)) = V;
}

void RenumF(int f1, int f2)
{
    Fragment F;
    int i;
    for (i=0; i<Vertices->num; i++)
        if (vertex(i)->fr == f1) vertex(i)->fr = f2;
        else if (vertex(i)->fr == f2) vertex(i)->fr = f1;

    for (i=0; i<Fragments->num; i++)
        if (fragment(i)->fr == f1) fragment(i)->fr = f2;
        else if (fragment(i)->fr == f2) fragment(i)->fr = f1;

    F = *(fragment(f1));
    *(fragment(f1)) = *(fragment(f2));
    *(fragment(f2)) = F;
}

void RenumE(int e1, int e2)
{
    Edge E;
    OutEdges->Delete(vertex(edge(e1)->from)->out, e1);
    OutEdges->AddLast(vertex(edge(e1)->from)->out, e2);

    InEdges->Delete(vertex(edge(e1)->to)->in, e1);
    InEdges->AddLast(vertex(edge(e1)->to)->in, e2);

    OutEdges->Delete(vertex(edge(e2)->from)->out, e2);
    OutEdges->AddLast(vertex(edge(e2)->from)->out, e1);

    InEdges->Delete(vertex(edge(e2)->to)->in, e2);
    InEdges->AddLast(vertex(edge(e2)->to)->in, e1);

    E = *(edge(e1));
    *(edge(e1)) = *(edge(e2));
    *(edge(e2)) = E;
}

int StrLen(const char* str)
{
    int i = 0;
    while (*str++) i++;
    return i;
}

void StrConcat(char* beg, const char* end)
{
    char* s = beg + StrLen(beg);
    StrCopy(end, s);
}

// Normalization ------------

int IsInside(int x, int y, int x1, int y1, int w1, int h1)
{
    if (x > x1 && y > y1 && x < x1 + w1 && y < y1 + h1) return 1;
    else return 0;
}

void SimpleRecMove(int fr, int dx, int dy)
{
    SetHiFrBit2(fr);

    int fx = fragment(fr)->x;
    int fy = fragment(fr)->y;
    int fw = fragment(fr)->w;
    int fh = fragment(fr)->h;

    // Move vertices and edge bends
    for (int i=0; i<Vertices->num; i++)
        if (fragment(vertex(i)->fr)->flags & 2)
        {
            vertex(i)->x += dx;
            vertex(i)->y += dy;

            // Move bends
            OutEdges->Get(vertex(i)->out);
            while (OutEdges->NoEnd())
            {
                int e = OutEdges->Next();
                LKEY nx = XBends->NewList();
                LKEY ny = YBends->NewList();
                XBends->Get(edge(e)->xbends);
                YBends->Get(edge(e)->ybends);
                while (XBends->NoEnd())
                {
                    int x = XBends->Next();
                    int y = YBends->Next();
                    if (IsInside(x, y, fx, fy, fw, fh))
                    {
                        XBends->AddLast(nx, x + dx);
                        YBends->AddLast(ny, y + dy);
                    }
                    else
                    {
                        XBends->AddLast(nx, x);
                        YBends->AddLast(ny, y);
                    }
                }
                XBends->DestList(edge(e)->xbends);
                YBends->DestList(edge(e)->ybends);
                edge(e)->xbends = nx;
                edge(e)->ybends = ny;
            }

            InEdges->Get(vertex(i)->in);
            while (InEdges->NoEnd())
            {
                int e = InEdges->Next();
                if (!(fragment(vertex(edge(e)->from)->fr)->flags & 2))
                {
                    LKEY nx = XBends->NewList();
                    LKEY ny = YBends->NewList();
                    XBends->Get(edge(e)->xbends);
                    YBends->Get(edge(e)->ybends);
                    while (XBends->NoEnd())
                    {
                        int x = XBends->Next();
                        int y = YBends->Next();
                        if (IsInside(x, y, fx, fy, fw, fh))
                        {
                            XBends->AddLast(nx, x + dx);
                            YBends->AddLast(ny, y + dy);
                        }
                        else
                        {
                            XBends->AddLast(nx, x);
                            YBends->AddLast(ny, y);
                        }
                    }
                    XBends->DestList(edge(e)->xbends);
                    YBends->DestList(edge(e)->ybends);
                    edge(e)->xbends = nx;
                    edge(e)->ybends = ny;
                }
            }
        }

    // Move fragments
    for (i=0; i<Fragments->num; i++)
        if (fragment(i)->flags & 2)
        {
            fragment(i)->x += dx;
            fragment(i)->y += dy;
        }
}

void F_AutoMove(int obj, int n)
{
    if (obj == 1 && !n) return;
    int m_up = 0;
    int m_down = 0;
    int m_left = 0;
    int m_right = 0;

    int x, y, w, h, ffr;
    switch (obj)
    {
    case 0:
        w = vertex(n)->w * 2;
        h = vertex(n)->h * 2;
        x = vertex(n)->x - vertex(n)->w;
        y = vertex(n)->y - vertex(n)->h;
        ffr = vertex(n)->fr;
        break;

    case 1:
        x = fragment(n)->x;
        y = fragment(n)->y;
        w = fragment(n)->w;
        h = fragment(n)->h;
        ffr = fragment(n)->fr;
        break;
    }

    ListBox* LB = new ListBox;
    LKEY k = LB->NewList();

    for (int i=0; i<Fragments->num; i++)
        if (fragment(i)->fr == ffr && (!obj || n != i))
        {
            int left = fragment(i)->x + fragment(i)->w - x + BB_Dist;
            int up = fragment(i)->y + fragment(i)->h - y + BB_Dist;
            int right = x + w - fragment(i)->x + BB_Dist;
            int down = y + h - fragment(i)->y + BB_Dist;
            int m1 = up;
            if (down < m1) m1 = down;
            if (right < m1) m1 = right;
            if (left < m1) m1 = left;
            LB->AddLast(k, 1);
            LB->AddLast(k, i);
            if (m1 == left)
            {
                LB->AddLast(k, 0);
                if (left > m_left) m_left = left;
            }
            else
                if (m1 == right)
                {
                    LB->AddLast(k, 1);
                    if (right > m_right) m_right = right;
                }
                else
                    if (m1 == up)
                    {
                        LB->AddLast(k, 2);
                        if (up > m_up) m_up = up;
                    }
                    else
                        if (m1 == down)
                        {
                            LB->AddLast(k, 3);
                            if (down > m_down) m_down = down;
                        }
        }

    for (i=0; i<Vertices->num; i++)
        if (vertex(i)->fr == ffr && (obj || i != n))
        {
            int left = vertex(i)->x + vertex(i)->w - x + BB_Dist;
            int up = vertex(i)->y + vertex(i)->h - y + BB_Dist;
            int right = x + w - vertex(i)->x + vertex(i)->w + BB_Dist;
            int down = y + h - vertex(i)->y + vertex(i)->h + BB_Dist;
            int m1 = up;
            if (down < m1) m1 = down;
            if (right < m1) m1 = right;
            if (left < m1) m1 = left;
            LB->AddLast(k, 0);
            LB->AddLast(k, i);
            if (m1 == left)
            {
                LB->AddLast(k, 0);
                if (left > m_left) m_left = left;
            }
            else
                if (m1 == right)
                {
                    LB->AddLast(k, 1);
                    if (right > m_right) m_right = right;
                }
                else
                    if (m1 == up)
                    {
                        LB->AddLast(k, 2);
                        if (up > m_up) m_up = up;
                    }
                    else
                        if (m1 == down)
                        {
                            LB->AddLast(k, 3);
                            if (down > m_down) m_down = down;
                        }
        }

    int need_move = m_up || m_down || m_left || m_right;
    if (!need_move) goto eee;

    LB->Get(k);
    while (LB->NoEnd())
    {
        int object = LB->Next();
        int i = LB->Next();
        int dir = LB->Next();
        int dx, dy;
        switch (dir)
        {
        case 0:
            dx = -m_left;
            dy = 0;
            break;
        case 1:
            dx = m_right;
            dy = 0;
            break;
        case 2:
            dx = 0;
            dy = -m_up;
            break;
        case 3:
            dx = 0;
            dy = m_down;
            break;
        }
        if (object) SimpleRecMove(i, dx, dy);
        else
        {
            vertex(i)->x += dx;
            vertex(i)->y += dy;
        }
    }

 eee:
    delete(LB);
}

void NormalizeEF(int fr, int x, int y, int red)
{
    if (!fragment(fr)->flags)
    {
        fragment(fr)->x = x;
        fragment(fr)->y = y;
        fragment(fr)->w = InitFrWidth;
        fragment(fr)->h = InitFrHeight;
        return;
    }

    for (int i=0; i<Fragments->num; i++)
        if (fragment(i)->fr == fr) NormalizeEF(i, x, y, red);
    for (i=0; i<Fragments->num; i++)
        if (fragment(i)->fr == fr) F_AutoMove(1, i);
    OptimizeFrLoc(fr, red);
}

void NormalizeFragment(int fr, int red)
{
    LKEY ef = GenLists->NewList();
    for (int i=0; i<Fragments->num; i++)
        if (fragment(i)->fr == fr)
            if (fragment(i)->flags == 1) NormalizeFragment(i, red);
            else
            {
                GenLists->AddLast(ef, i);
                fragment(i)->fr = -1;
            }

    for (i=0; i<Fragments->num; i++)
        if (fragment(i)->fr == fr) F_AutoMove(1, i);

    for (i=0; i<Vertices->num; i++)
        if (vertex(i)->fr == fr) F_AutoMove(0, i);
    OptimizeFrLoc(fr, red);

    GenLists->Get(ef);
    if (GenLists->NoEnd())
    {
        while (GenLists->NoEnd())
        {
            int n = GenLists->Next();
            fragment(n)->fr = fr;
            NormalizeEF(n, fragment(fr)->x, fragment(fr)->y, red);
        }
        GenLists->Get(ef);
        while (GenLists->NoEnd())
            F_AutoMove(1, GenLists->Next());
        OptimizeFrLoc(fr, red);
    }
    GenLists->DestList(ef);
}

int SetEmpFlags(int fr)
{
    if (fragment(fr)->flags == 0) return 1;
    int empty = 1;
    for (int i=0; i<Fragments->num; i++)
        if (fragment(i)->fr == fr) empty &= SetEmpFlags(i);
    for (i=0; i<Vertices->num; i++)
        if (vertex(i)->fr == fr) empty = 0;
    if (empty) fragment(fr)->flags = 2;
    return empty;
}

void NormalizeGraph(int red)
{
    for (int i=0; i<Fragments->num; i++)
        fragment(i)->flags = 0;

    for (i=1; i<Fragments->num; i++)
        fragment(fragment(i)->fr)->flags = 1;

    for (i=0; i<Vertices->num; i++)
        fragment(vertex(i)->fr)->flags = 1;

    SetEmpFlags(0);
    NormalizeFragment(0, red);

    int dx = fragment(0)->x;
    int dy = fragment(0)->y;

    for (i=0; i<Fragments->num; i++)
    {
        fragment(i)->x -= dx;
        fragment(i)->y -= dy;
    }

    for (i=0; i<Vertices->num; i++)
    {
        vertex(i)->x -= dx;
        vertex(i)->y -= dy;
    }         

    for (i=0; i<Edges->num; i++)
    {
        XBends->Get(edge(i)->xbends);
        YBends->Get(edge(i)->ybends);
        LKEY nx = XBends->NewList();
        LKEY ny = YBends->NewList();
        while (XBends->NoEnd())
        {
            XBends->AddLast(nx, XBends->Next() - dx);
            YBends->AddLast(ny, YBends->Next() - dy);
        }
        XBends->DestList(edge(i)->xbends);
        YBends->DestList(edge(i)->ybends);
        edge(i)->xbends = nx;
        edge(i)->ybends = ny;
    }
}

//-------------- Connection ----------

char* SampleMask;
int MaskNPos;

HANDLE OutPipe;
HANDLE InPipe;
HWND HigresWin;

long StepsDone;
long LSStep; // number of step on which the last sample was created
int SamplesLeft; // number of samples e.p. must create before stop
                  // (-1 - don't stop)
int SPS;     // steps per sample
int IsTerm;  // 1 if system closes e.p. (used by EndAlg)

DWORD Pipe_Written_Bytes; // number of bytes written to the
    // out-pipe last time (by WritePipe macro)
DWORD Pipe_Read_Bytes;

void PrepareSampleFilename(long n)
{
    long l0 = 456976;
    while (n >= l0) n -= l0;
    long l1 = 17576;
    long l2 = 676;
    long l3 = 26;
    char t2, t3;
    char t1 = t2 = t3 = 'a';
    while (n >= l1) { n -= l1; t1++; }
    while (n >= l2) { n -= l2; t2++; }
    while (n >= l3) { n -= l3; t3++; }
    char* s = SampleMask + MaskNPos;
    *s++ = t1;
    *s++ = t2;
    *s++ = t3;
    *s = (char)(n + 'a');
}

void PostProcMessage()
{
    PostMessage(HigresWin, WM_COMMAND, 750, 0);
}

int WaitCommand();

int StrToLong(const char* str, long& l)
{
    signed char s = 1; char ch = 32;
    while (ch == 32) { ch = *str; str++; }
    str--;
    if (!ch) return 8;
    if (ch == '-') { s = -1; str++; ch = *str; }
    if (ch == '+') { str++; ch = *str; }
    l = 0;
    while ((ch >= '0') && (ch <= '9'))
    {
        if (l > 214748363) return 16;
        l = l * 10 + ch - '0';
        str++;
        ch=*str;
    }
    l *= s;
    if ((ch != 0) && (ch != ' ') && (ch != 13)) return 4;
    return 0;
}

void ShowLastError()
{
    char* lpMsgBuf;
    FormatMessage( FORMAT_MESSAGE_ALLOCATE_BUFFER |
        FORMAT_MESSAGE_FROM_SYSTEM, NULL, GetLastError(),
        MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), // Default language
        (LPTSTR) &lpMsgBuf, 0, NULL);

    // Display the string.
    MessageBox(NULL, lpMsgBuf, "GetLastError", MB_OK|MB_ICONINFORMATION );
    // Free the buffer.
    LocalFree(lpMsgBuf);
}

#define WritePipe(buf, n) WriteFile(OutPipe, buf, n, &Pipe_Written_Bytes, NULL)
#define ReadPipe(buf, n) ReadFile(InPipe, buf, n, &Pipe_Read_Bytes, NULL)

int StartAlg(const char* lpCmdLine)
{
    char b[1024];

    StrCopy(lpCmdLine, b);
    char* s = b;
    while (*s != ' ')
    {
        if (!(*s)) { IsTerm = 3; return 3; }
        s++;
    }
    *s++ = 0;
    if (!StrEq(b, "higres_3xx_module_start"))
    {
        IsTerm = 3;
        return 3;
    }

    // get in-pipe handle
    char* s1 = s;
    while (*s1 != ' ')
    {
        if (!(*s1)) { IsTerm = 3; return 3; }
        s1++;
    }
    *s1++ = 0;
    long l;
    if (StrToLong(s, l)) { IsTerm = 3; return 3; }
    InPipe = (HANDLE)l;

    // get out-pipe handle
    s = s1;
    while (*s1 != ' ')
    {
        if (!(*s1)) { IsTerm = 3; return 3; }
        s1++;
    }
    *s1++ = 0;
    if (StrToLong(s, l)) { IsTerm = 3; return 3; }
    OutPipe = (HANDLE)l;

    // get Higres window handle
    s = s1;
    while (*s1 != ' ')
    {
        if (!(*s1)) { IsTerm = 3; return 3; }
        s1++;
    }
    *s1++ = 0;
    if (StrToLong(s, l)) { IsTerm = 3; return 3; }
    HigresWin = (HWND)l;

    // get sample mask and find MaskNPos
    SampleMask = new char[1024];
    StrCopy(s1, SampleMask);
    MaskNPos = StrLen(SampleMask) - 8;

    BeginWork();

    StepsDone = 0;
    LSStep = 0;
    SPS = 1;
    IsTerm = 0;

    PrepareSampleFilename(0);
    int r = LoadGraph(SampleMask);
    if (r)
    {
        int n = 3;
        WritePipe(&n, 4);
        WritePipe("Error!", 7);
        n = 0;
        WritePipe(&n, 4);
    }
    else
    {
        int n = 1;
        WritePipe(&n, 4);
        WritePipe(&par_num, 4);
        for (int i=0; i<par_num; i++)
        {
            WritePipe(&(param[i].d_type), 4);
            char* s = GenStrings->Get(param[i].name);
            WritePipe(s, StrLen(s) + 1);
            switch (param[i].d_type)
            {
            case 1:
            case 2:
                WritePipe(param[i].p, 4);
                break;
            case 3:
                WritePipe(param[i].p, 8);
                break;
            case 4:
                WritePipe(param[i].p, StrLen((char*)param[i].p) + 1);
                break;
            }
        }
    }

    PostProcMessage();
    if (r) { IsTerm = r + 3; return IsTerm; }
    else return WaitCommand();
}

 void EndAlg()
{
    if (IsTerm == 2 || IsTerm == 3)
    {
        if (par_num) delete[](param);
        if (GenStrings) delete(GenStrings);
        return;
    }
    
    if (IsTerm == 4 || IsTerm == 5)
    {
        if (par_num) delete[](param);
        EndWork();
        return;
    }
    
    // create last sample
    if (StepsDone != LSStep)
    {
        PrepareSampleFilename(StepsDone);
        SaveGraph(SampleMask);
        int n = 7;
        WritePipe(&n, 4);
        WritePipe(&StepsDone, 4);
        PostProcMessage();
    }

    // post last message
    int n;
    if (IsTerm) n = 6; else n = 5;
    WritePipe(&n, 4);
    PostProcMessage();

    delete[](SampleMask);
    if (par_num) delete[](param);

    EndWork();
}


 int WaitCommand()
{
    int n, i, m;

    while (1)
    {

    ReadPipe(&n, 4);

    switch (n)
    {
    case 6:
        m = 8;
        WritePipe(&m, 4);
        for (i=0; i<par_num; i++)
            switch (param[i].d_type)
            {
            case 1:
            case 2:
                WritePipe(param[i].p, 4);
                break;
            case 3:
                WritePipe(param[i].p, 8);
                break;
            case 4:
                WritePipe(param[i].p, StrLen((char*)param[i].p) + 1);
                break;
            }
        PostProcMessage();
        break;

    case 5:
        IsTerm = 1;
        return 1;
    
    case 2:
        ReadPipe(&SamplesLeft, 4);
        if (SamplesLeft == 0) SamplesLeft = -1;
        ReadPipe(&SPS, 4);
        return 0;

    case 1:
        ReadPipe(&i, 4);
        switch (param[i].d_type)
        {
        case 1:
        case 2:
            ReadPipe(param[i].p, 4);
            break;
        case 3:
            ReadPipe(param[i].p, 8);
            break;
        case 4:
            char* str = (char*)(param[i].p);
            do ReadPipe(str, 1); while (*str++);
            break;
        }
        break;
    }

    }
}

 int EndStep()
{
    StepsDone++;
    if (StepsDone == LSStep + SPS)
    {
        LSStep = StepsDone;
        PrepareSampleFilename(StepsDone);
        SaveGraph(SampleMask);
        if (SamplesLeft != -1) SamplesLeft--;
        int n = 7;
        WritePipe(&n, 4);
        WritePipe(&StepsDone, 4);
        PostProcMessage();
        Sleep(0);
    }
    
    if (IsTerm) return 1;

    // Send the 'ping' message and process all system
    // messages that are stored in the pipe untill we meet the
    // reply to the 'ping' message we sent.
    int n = 9;
    WritePipe(&n, 4);
    PostProcMessage();
    do
    {
        ReadPipe(&n, 4);
        if (n == 5) { IsTerm = 1; return 1; }
        if (n == 4)
        {
            int m = 4;
            WritePipe(&m, 4);
            PostProcMessage();
            return WaitCommand();
        }
    }
    while (n != 9); // 9 is a reply to 'ping' message

    if (SamplesLeft == 0)
    {
        int n = 4;
        WritePipe(&n, 4);
        PostProcMessage();
        return WaitCommand();
    }

    return 0;
}

void RegisterParameter(char* name, int* p)
{
    if (!par_num) param = new PARAMINFO[256];
    if (!GenStrings) GenStrings = new StringBox; 
    param[par_num].name = GenStrings->Put(name);
    param[par_num].d_type = 1;
    param[par_num++].p = (void*)p;
}

void RegisterParameter(char* name, double* p)
{
    if (!par_num) param = new PARAMINFO[256];
    if (!GenStrings) GenStrings = new StringBox;
    param[par_num].name = GenStrings->Put(name);
    param[par_num].d_type = 3;
    param[par_num++].p = (void*)p;
}

void RegisterParameter(char* name, char* p)
{
    if (!par_num) param = new PARAMINFO[256];
    if (!GenStrings) GenStrings = new StringBox;
    param[par_num].name = GenStrings->Put(name);
    param[par_num].d_type = 4;
    param[par_num++].p = (void*)p;
}

void SendMesLog(const char* mes)
{
    int n = 3;
    WritePipe(&n, 4);
    WritePipe(mes, StrLen(mes) + 1);
    PostProcMessage();
}

void MesUser(const char* mes, int mes_type)
{
    int n = 2;
    WritePipe(&n, 4);
    WritePipe(&mes_type, 4);
    WritePipe(mes, StrLen(mes) + 1);
    PostProcMessage();
    ReadPipe(&n, 4);
    if (n != 3) SendMesLog("Connection error!");
}

int AskUserYN(const char* mes)
{
    int n = 2;
    WritePipe(&n, 4);
    n = 3;
    WritePipe(&n, 4);
    WritePipe(mes, StrLen(mes) + 1);
    PostProcMessage();
    ReadPipe(&n, 4);
    if (n != 3) SendMesLog("Connection error!");
    ReadPipe(&n, 4);
    return n;
}

int AskUserOC(const char* mes)
{
    int n = 2;
    WritePipe(&n, 4);
    n = 4;
    WritePipe(&n, 4);
    WritePipe(mes, StrLen(mes) + 1);
    PostProcMessage();
    ReadPipe(&n, 4);
    if (n != 3) SendMesLog("Connection error!");
    ReadPipe(&n, 4);
    return n;
}

 void CreateSampleXXX()
{
    if (StepsDone != LSStep)
    {
        PrepareSampleFilename(StepsDone);
        SaveGraph(SampleMask);

        int n = 7;
        WritePipe(&n, 4);
        WritePipe(&StepsDone, 4);
        PostProcMessage();
    }
}

void AskUser(const char* mes, int& i)
{
    if (IsTerm) return;

    CreateSampleXXX();

    int n = 2;
    WritePipe(&n, 4);
    n = 5;
    WritePipe(&n, 4);
    WritePipe(mes, StrLen(mes) + 1);
    WritePipe(&i, 4);
    PostProcMessage();
    ReadPipe(&n, 4);
    if (n == 5) IsTerm = 1;
    else
        if (n != 3)
        {
            IsTerm = 1;
            SendMesLog("Connection error!");
        }
    ReadPipe(&i, 4);
}

void AskUser(const char* mes, double& f)
{
    if (IsTerm) return;

    CreateSampleXXX();

    int n = 2;
    WritePipe(&n, 4);
    n = 7;
    WritePipe(&n, 4);
    WritePipe(mes, StrLen(mes) + 1);
    WritePipe(&f, 8);
    PostProcMessage();
    ReadPipe(&n, 4);
    if (n == 5) IsTerm = 1;
    else
        if (n != 3)
        {
            IsTerm = 1;
            SendMesLog("Connection error!");
        }
    ReadPipe(&f, 8);
}

void AskUser(const char* mes, char* buf, int len)
{
    if (IsTerm) return;

    CreateSampleXXX();

    int n = 2;
    WritePipe(&n, 4);
    n = 8;
    WritePipe(&n, 4);
    WritePipe(mes, StrLen(mes) + 1);
    WritePipe(buf, StrLen(buf) + 1);
    PostProcMessage();
    ReadPipe(&n, 4);
    if (n == 5) IsTerm = 1;
    else
        if (n != 3) SendMesLog("Connection error!");
        else
        {
            char* s = buf;
            char t;
            do
            {
                ReadPipe(&t, 1);
                if (len)
                {
                    *s++ = t;
                    len--;
                }
            }
            while (t);
        }
}

int AskVertexNumber(const char* mes)
{
    if (IsTerm) return 0;

    CreateSampleXXX();

    int n = 2;
    WritePipe(&n, 4);
    n = 9;
    WritePipe(&n, 4);
    WritePipe(mes, StrLen(mes) + 1);
    PostProcMessage();
    ReadPipe(&n, 4);
    if (n == 5)
    {
        IsTerm = 1;
        return 0;
    }
    else
        if (n != 3)
        {
            IsTerm = 1;
            SendMesLog("Connection error!");
            return 0;
        }
    int vn;
    ReadPipe(&vn, 4);
    return vn;
}

int AskFragmentNumber(const char* mes)
{
    if (IsTerm) return 0;

    CreateSampleXXX();

    int n = 2;
    WritePipe(&n, 4);
    n = 10;
    WritePipe(&n, 4);
    WritePipe(mes, StrLen(mes) + 1);
    PostProcMessage();
    ReadPipe(&n, 4);
    if (n == 5)
    {
        IsTerm = 1;
        return 0;
    }
    else
        if (n != 3)
        {
            IsTerm = 1;
            SendMesLog("Connection error!");
            return 0;
        }
    int vn;
    ReadPipe(&vn, 4);
    return vn;
}

int AskEdgeNumber(const char* mes)
{
    if (IsTerm) return 0;

    CreateSampleXXX();

    int n = 2;
    WritePipe(&n, 4);
    n = 11;
    WritePipe(&n, 4);
    WritePipe(mes, StrLen(mes) + 1);
    PostProcMessage();
    ReadPipe(&n, 4);
    if (n == 5)
    {
        IsTerm = 1;
        return 0;
    }
    else
        if (n != 3)
        {
            IsTerm = 1;
            SendMesLog("Connection error!");
            return 0;
        }
    int vn;
    ReadPipe(&vn, 4);
    return vn;
}

// Drawing methods

char* DrawOutFile;
int UseDefaultOptions;
int StartReturnVal;

int StartDrawMethod(const char* lpCmdLine)
{
    char b[1024];
    StrCopy(lpCmdLine, b);
    char ifn[1024];
    char* s = b;
    char* s1;
    UseDefaultOptions = 0;
    HigresWin = NULL;

nnn:
    // get input file name to ifn
    s1 = ifn;
    if (*s == '"')
    {
        s++;
        while (*s && *s != '"') *s1++ = *s++;
        if (*s) do s++; while (*s <= 32 && *s > -1);
        *s1 = 0;
    }
    else
    {
        while (*s > 32 || *s < 0) *s1++ = *s++;
        if (*s) do s++; while (*s <= 32 && *s > -1);        
        *s1 = 0;
    }

    if (!*s)
    {
        StartReturnVal = 10;
        return 10;
    }

    // if an option is specified
    if (ifn[0] == '-')
    {
        long l;
        switch (ifn[1])
        {
        case 'd':
            UseDefaultOptions = 1;
            break;

        case 'w':
            StrToLong(ifn + 2, l);
            HigresWin = (HWND)l;
            break;
        }
        goto nnn;
    }

    DrawOutFile = new char[1024];
    DrawOutFile[0] = 0;
    s1 = DrawOutFile;
    // get output file name to DrawOutFile
    if (*s == '"')
    {
        s++;
        while (*s && *s != '"') *s1++ = *s++;
        if (*s) do s++; while (*s <= 32 && *s > -1);
        *s1 = 0;
    }
    else
    {
        while (*s > 32 || *s < 0) *s1++ = *s++;
        if (*s) do s++; while (*s <= 32 && *s > -1);        
        *s1 = 0;
    }

    if (DrawOutFile[0] == 0)
    {
        StartReturnVal = 10;
        return 10;
    }

    BeginWork();
    StartReturnVal = LoadGraph(ifn);
    return StartReturnVal;
}

void SetDrawCountPC(int pc)
{
    if (HigresWin)
        PostMessage(HigresWin, WM_COMMAND, 1000 + pc, 0);
}

int EndDrawMethod(int is_success)
{
    int r = 0;
    if (!StartReturnVal && is_success) r = SaveGraph(DrawOutFile);
    if (StartReturnVal != 10) EndWork();
    if (HigresWin)
        PostMessage(HigresWin, WM_COMMAND, 1101, 0);
    return r;
}

Vertex* GetVertex(int vertex)
{
    return Vertices->Acc(vertex);
}

Edge* GetEdge(int edge)
{
    return Edges->Acc(edge);
}

void SetTypeShape ( int typenum, int newShape )
{
    type[typenum].shape = newShape;
}

void SetTypeLV_PAR ( int typenum, int newLV_PAR )
{
    type[typenum].lv_par = newLV_PAR;
}

void SetTypeB_STYLE ( int typenum, int newb_style )
{
    type[typenum].b_style = newb_style;
}
