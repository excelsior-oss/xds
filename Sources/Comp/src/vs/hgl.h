
// HGL 3.0 header

#include <Windows.h>

typedef void CFWindow;

// Dinamic data structures

template <class T>
class DinArr
{
    public:

    DinArr() { num = 0; };
    ~DinArr();
    int Add(T x);
    int Delete(int n);
    T* Acc(int n) { return &(tab[n>>8][n&255]); };

    int num;
    T* tab[128];
};

struct Stack
{
    int* data;
    int top;

    public:

    Stack() { data = new int[4096]; top = 0; };
    ~Stack() { delete[](data); };
    void Push(int i) { data[top++] = i; };
    void Pop(int& i) { i = data[--top]; };
    int num() { return top; };
};

typedef unsigned int LKEY;

struct ListIt
{
    union { LKEY end; int n; };
    LKEY next;
};

class ListBox
{
    unsigned int fbound; // First clear element in data array.
    unsigned int cur_el; // Current element in data array.
    int empty_sign;
    ListIt* data[256];
    unsigned int dbn;    // Number of 256-bytes data blocks used.
    Stack* cstack;       // Stack for current elements

    public:

    ListBox();
    ~ListBox();
    int Get(LKEY key);   // Open the list with this key. Returns zero if
                        // this list is empty.
    int Next();          // Get next value in opened list. If the list is at the
                        // end returns empty_sign.
    void SetEmptySign(int sign);
    int NoEnd() { return cur_el; }; // Returns non-zero value if opened list is
                        // not at the end, otherwise returns zero.
    LKEY NewList();      // Creates new empty list.
    void DestList(LKEY key); // Deletes list.
    void AddFirst(LKEY key, int n); // Adds n to the beginning of list.
    void AddLast(LKEY key, int n);  // Adds n to the end of list.
    int AddFirstX(LKEY key, int n); // Adds n if it was not in list and returns
                                    // 1, otherwise returns 0.
    int AddLastX(LKEY key, int n);
    int Delete(LKEY key, int n);
    int DeleteFirst(LKEY key);
    int Search(LKEY key, int n); // Returns how many times n present in list.
    void Insert(LKEY key, int prev_num, int n);
    void Change(LKEY key, int num, int n);
    void DeleteByNum(LKEY key, int num);
    int GetLast(LKEY key);
    void PushCur() { cstack->Push((int)cur_el); };
    void PopCur() { int x; cstack->Pop(x); cur_el = x; };
};

typedef unsigned int SKEY;

unsigned const SB_LEN = 4096;

struct DAdr
{
    int a_n;
    unsigned int adr;
};

class StringBox
{
    char* arr[128];  // Addresses of character arrays
    unsigned int arr_num; // Number of arrays currently used
    DAdr* d_adr[128];
    unsigned int d_adr_num;

    unsigned int cur_arr;
    unsigned int cur_adr;
    SKEY ek; // empty key

    char null;

    public:

    StringBox();
    ~StringBox();
    SKEY Put(const char* str);
    char* Get(SKEY key);
    void Delete(SKEY key);
    char* FindPlaceInCurrenArray(int len);
};

// Basic structures

struct Vertex
{
    int type;
    int x, y;
    int w, h;
    int fr;   // Number of fragment this vertex belongs to.
    int tn;   // Number of this vertex in type numeration.
    int dx;   // Anchor point of external labels text
    int dy; 
    unsigned int flags; // BIT 1 -- Used in multigraph checking.
    LKEY in;            // Key of list of incoming edges.
    LKEY out;           // Key of list of outgoing edges.
};

struct Fragment
{
    int type;
    int x, y, w, h;
    int scroll_x, scroll_y;
    RECT WinRect;
    int scale;
    int fr;     // Number of external fragment.
    int tn;     // Number of this fragment in type numeration.
    int kind;   // 0 - closed, 1 - open.
    int level;
    int dx;     // Anchor point of labels text when open
    int dy; 
    long ID;
    unsigned int flags; // Used somewhere
    int draw;           // Used in drawing process.
    SKEY title;         // (In general stringbox).
    CFWindow* ptw;
};

struct Edge
{
    int type;
    int from;
    int to;
    int tn;
    int dx1, dy1, dx2, dy2; // Anchor points vector coordinates.
    int lpt;                // Label stick point parameter.
    int ort;        // BIT 0 - orthogonalization at starting vertex
                // BIT 1 - orthogonalization at end vertex
    unsigned int flags;     // Currently not used.
    LKEY xbends;            // List of x-coordinates of trace points.
    LKEY ybends;            // List of y-coordinates of trace points.
};

struct Label
{
    void GetValString(int n, char* str);
    void GetDefValString(char* str);
    int SetVal(int n, char* str);
    int SetDefVal(char* str);
    void SetValFromDef(int n);

    int data_type; // 1 - integer, 3 - float, 4 - text;
    int i_defval;
    double f_defval;
    int par;       // BIT 0 - Left allign.
    int width;
    int prec;
    SKEY t_defval; // In General StringBox.
    SKEY name;     // In General StringBox.
    DinArr<int>* i_val;
    DinArr<double>* f_val;
    StringBox* text;
};

struct Type
{
    int AddTObject();
    int DeleteTObject(int m);
    void AddLabel(int t);
    void DeleteLabel(int ln);

    int SetVisCode(char* str, int is_x);
    void GetVisCode(char* str, int is_x);
    void DestVisCode(int is_x);

    int object;   // 0 - vertex, 1 - fragment, 2 - edge.
    int shape;    // For vertices:
                // 0 - rectangle, 1 - rounded rectangle,
                // 2 - ellipse, 3 - rhombus.
                // For edges:
                // 0 - polyline,
                // 2 - rounded.

    int b_style;  // Border style (vert.) or line style (edge):
                // Border styles:
                // 0 - solid
                // 1 - bold
                // 2 - extra bold
                // 3 - dotted
                // 4 - double solid
                // 5 - bold & solid
                // Only cases 0, 1, 2, 3 are available for line style

    long b_color; // Border color (v) or line color (e) or interior color (o.f.).
    long i_color; // Color of the interior of vertex or closed fragment.
    int defwidth, defheight; // Only for vertices.
    int def_ort; // Only for edges

    int num;      // Number of objects of this type.

    LOGFONT lf;   // Font to print labels text of edges,
                // internal l.t. of vertices, or
                // closed l.t. of fragments.
    LOGFONT lfx;  // Font to print external l.t. of vertices
                // or open l.t. of fragments.

    int lv_par;   // BIT 0 - Use back color (for edges and external vertex labels),
                // BIT 1 - Allign center (lf),
                // BIT 2 - Allign top left (lf),
                // BIT 3 - Skip empty lines (lf),
                // BIT 4 - Allign center (lfx),
                // BIT 5 - Allign top left (lfx),
                // BIT 6 - Skip empty lines (lfx).
                // BIT 7 - Auto adjust sizes (vert.)
                // BIT 8 - Reduce only to defaults (vert.)

    long l_col;   // Labels text color (lf).
    long l_colx;  // Labels text color (lfx).

    int Lab_Dist; // Distance between the vertex border and the external labels text
                // or distance between the open fragment border and lab. text
                // or distance between the stick point and the labels text

    int Arrow_wid; // width par. of arrow (for edges)
    int Arrow_len;
    int ArrSScale; // 1 if arrow sizes depend on scale, 0 - otherwise
    int ArrShape;  // 0 - simple, 1 - triangle, 2 - special

    LKEY vis_cd;  // First (lf) visualization code. (In GenLists)
    LKEY vis_cdx; // Second (lfx) visualization code. (In GenLists)

    // Drawing tools:

    HFONT font;   // lf
    HFONT fontx;  // lfx
    HPEN pen;     // Pen for border of vertices, lines of edges and
                // top left border of fragments
                // Created according to b_style and b_color (for v. & e.)
                // i_color (for fr.) 
    HPEN penx;    // Pen for bottom right fragment border or
                // pen for drawing edge arrows (if needed) or
                // pen for drawing the internal part of 'double' styled vertex border
    HBRUSH brush; // Brush for interial of vertices and fragments
                // or arrows of edge (if needed)

    HBRUSH brushy; // Created only for fragments (open)
    HPEN peny;
    HPEN penxy;
    //---------------

    SKEY name;    // (In general stringbox).
    DinArr<Label>* Labels;
    int ToolsCreated; // 1 if drawing tools are currently created
                    // 0 -- otherwise
    int CurFontScale; // Current font scale (the scale last time
                    // passed to CreateDTools function).
};

struct PARAMINFO
{
    int d_type;
    SKEY name;
    void* p;
};

// Variables

extern int GrType; // Bit 0 - (set - not oriented, reset - oriented)
                // Bit 1 - graph / multigraph
                // Bit 2 - alt fr / random fr
extern unsigned int GGenOpt; // Bits:
                            // 0 - 2  Hanging edges par.
                            // 3      ---
                            // 4      Rounded rect. roundness depend on size.
                            // 5      ---
                            // 6      ---
                            // 7      ---
                            // 8      Border width depends on scale.
                            // 9      ---
                            // 10     Self fragment par.: 0 -- hide,
                            // 11     1 -- show border, 2 -- full show
                            // 12     ---
                            // 13     ---
                            // 14     Allow line breaks in labels

extern int Grid_w;
extern int Grid_h;
extern int Grid_x;
extern int Grid_y;
extern int Ex;
extern DinArr<Vertex>* Vertices;
extern DinArr<Fragment>* Fragments;
extern DinArr<Edge>* Edges;
extern Type* type;
extern int typ_num;      // Number of types of all objects.
extern Stack* EUStack;
extern int EUSwitch;
extern int EUNum;
extern ListBox* InEdges;  // ListBox of incoming edges for all nodes.
extern ListBox* OutEdges;
extern ListBox* GenLists; // General lists.
extern ListBox* GenX;     // General lists of coordinates
extern ListBox* GenY;
extern StringBox* GenStrings;
extern ListBox* XBends;  // Edges bends lists.
extern ListBox* YBends; 
extern int RRPar;    // Rounded rectangles parameter.
extern LOGFONT DefLogFont;
extern long Next_F_ID;
extern PARAMINFO* param;
extern int par_num;
extern int UseDefaultOptions;
       
// Functions

// 1. Add/delete vertex/edge

Vertex* GetVertex(int vertex);
Edge* GetEdge(int edge);
void SetTypeShape ( int typenum, int newShape );
void SetTypeB_STYLE ( int typenum, int newb_style );
void SetTypeLV_PAR ( int typenum, int newLV_PAR );

void AddVertex(int t, int fr, int x, int y); 
    // t -- number of type
    // fr -- number of fragment to which the new vertex will belong.
void DeleteVertex(int n);

void AddEdge(int t, int from, int to);
void DeleteEdge(int n);

// 2. Fragment hierarchy functions

void CreateFragment(int t, int fr, int x, int y, int w, int h);
void Unfold(int fr);
void MoveVertexToFragment(int v, int fr);
void OptimizeFrLoc(int fr, int BB_Dist = 4);

// 3. Types

int AddType(int object, char* name);
    // Parameters:
    // object -- the kind of objects of the new type (vertex, fragment, edge)
    // name -- name of the new type
    // Returns:
    // 1 -- type with this name is already exist
    // 0 -- succes

int DeleteType(int tn, int par = 0);
    // Parameters:
    // tn -- number of type to delete
    // if par != 0 then this type will be deleted even if it is the last
    // vertex (fragment, edge) type
    // Returns:
    // 0 -- succes
    // 1 -- this is the last node type
    // 2 -- last fragment type
    // 3 -- last edge type
    // 4 -- objects of this type are present in the graph

int RenameType(int tn, char* name);
    // Returns:
    // 1 -- there is another type with this name
    // 0 -- succes

// 4. Start/End

void BeginWork();
void CreateNewGraph(int t);
    // Parameters:
    // t -- number of fragment type for the main fragment of the new graph
void DestroyGraph();
void EndWork();

// 5. In/Out

int LoadGraph(char* filename);
int SaveGraph(char* filename);

// 6. Labels

int AddLabel_Ex(int t, int d);
    // Parameters:
    // t -- type to which the label will be added
    // d -- data type of this label (1 - int, 3 - float, 4 - string)
    // Returns:
    // Number of added label

void DeleteLabel(int t, int ln);
    // Parameters:
    // t -- type which label will be removed
    // ln -- number of label which will be removed

int SetVisCode_Ex(int t, char* str, int is_x);
    // Parameters:
    // t -- number of type
    // str -- string with vis. code
    // is_x -- not zero to set extended vis code
    // Returns:
    // 0 -- succes
    // 1 -- wrong vis. code

void GetVisCode(int t, char* str, int is_x);
    // Parameters:
    // t -- number of type
    // str -- string to receive the vis. code
    // is_x -- not zero to get extended vis code

int GetLabVal(int obj, int n, int ln, char* str);
    // Parameters:
    // obj -- kind of object which label is to be retreived 
    // n -- number of object
    // ln -- number of label to retrieve
    // str -- string to receive the label value 
    // Returns:
    // 0 -- success
    // 1 -- there is no label with this number

int GetLabDefVal(int t, int ln, char* str);
    // Parameters:
    // t -- number of type
    // ln -- number of label
    // str -- string to receive the default label value
    // Returns:
    // 0 -- success
    // 1 -- there is no label with this number

int SetLabVal(int obj, int n, int ln, char* str);
    // Parameters:
    // obj -- kind of object which label is to be set 
    // n -- number of object
    // ln -- number of label to set
    // str -- string with the label value
    // Returns:
    // 0 -- success
    // 1 -- there is no label with this number

int SetLabDefVal(int t, int ln, char* str);
    // Parameters:
    // t -- number of type
    // ln -- number of label
    // str -- string with the default label value
    // Returns:                       
    // 0 -- success
    // 1 -- there is no label with this number

int ResetLabVal(int obj, int n, int ln);
    // Parameters:
    // obj -- kind of object which label is to be reset (set to default)
    // n -- number of object
    // ln -- number of label to reset
    // Returns:
    // 0 -- success
    // 1 -- there is no label with this number

//int SetLabVal(int obj, int n, int ln, int val);
    // Parameters:
    // obj -- kind of object which label is to be set 
    // n -- number of object
    // ln -- number of label to set
    // val -- value of label 
    // Returns:
    // 0 -- success
    // 1 -- there is no label with this number

//int SetLabVal(int obj, int n, int ln, double val);

int GetLabVal(int obj, int n, int ln, int& val);
    // Parameters:
    // obj -- kind of object which label is to be retreived 
    // n -- number of object
    // ln -- number of label to retrieve
    // val -- label value
    // Returns:
    // 0 -- success
    // 1 -- there is no label with this number
    // 2 -- label datatype not compatible with integer

int GetLabVal(int obj, int n, int ln, double& val);

int SetLabDefVal(int t, int ln, int val);
    // Parameters:
    // t -- number of type
    // ln -- number of label
    // val -- default label value
    // Returns:
    // 0 -- success
    // 1 -- there is no label with this number

int SetLabDefVal(int t, int ln, double val);

int GetLabDefVal(int t, int ln, int& val);
    // Parameters:
    // t -- number of type
    // ln -- number of label
    // val -- default label value
    // 0 -- success
    // 1 -- there is no label with this number
    // 2 -- label datatype is not compatible with integer

int GetLabDefVal(int t, int ln, double& val);

// 7. Bends

void DeleteAllBends(int n);
    // Parameters:
    // n -- number of edge which bends must be removed

void AddBendFirst(int n, int x, int y);
    // Parameters:
    // n -- number of edge to which the new bend must be added
    // x -- x-coordinate of the bend point
    // y -- y-coordinate of the bend point

void AddBendLast(int n, int x, int y);
    // Parameters:
    // n -- number of edge to which the new bend must be added
    // x -- x-coordinate of the bend point
    // y -- y-coordinate of the bend point

void InsertBend(int n, int bn, int x, int y);
    // Parameters:
    // n -- number of edge to which the new bend must be inserted
    // bn -- number of bend after which the new one will be placed
    //       (anchor point at the starting vertex is considered to be
    //        the bend number zero)
    // x -- x-coordinate of the bend point
    // y -- y-coordinate of the bend point

void DeleteBend(int n, int bn);
    // Parameters:
    // n -- number of edge
    // bn -- number of bend to delete (numeration starts from 1)

void DeleteFirstBend(int n);
    // Parameters:
    // n -- number of edge

// 8. Access functions

void GetOutEdges(int n);
    // Parameters:
    // n -- number of vertex which outgoing edges must be accessed

int NextOutEdge(int& e);
    // Parameters:
    // e -- number of the next edge
    // Returns:
    // 0 -- no more edges (e is invalid)
    // 1 -- other case

void GetInEdges(int n);
    // Parameters:
    // n -- number of vertex which incoming edges must be accessed

int NextInEdge(int& e);
    // Parameters:
    // e -- number of the next edge
    // Returns:
    // 0 -- no more edges (e is invalid)
    // 1 -- other case

// For undirected graphs
void GetEdges(int n);
int NextEdge(int& e);

// Push/Pop functions for recursive use of edge lists
void PushOutEdge();
void PushInEdge();
void PushEdge();
void PopOutEdge();
void PopInEdge();
void PopEdge();

void GetBends(int n);
    // Parameters:
    // n -- number of edge which bends must be accessed

int NextBend(int& x, int& y);
    // Parameters:
    // x -- x-coordinate of the bend point 
    // y -- y-coordinate of the bend point
    // Returns:
    // 0 -- no more bends (x and y are invalid)
    // 1 -- other case

// 9. Other functions

void SetFrTitle(int fr, char* str);
    // Parameters:
    // fr -- number of fragment which title must be changed
    // str -- string with the new title

char* GetFrTitle(int fr);

void SetLabelName(int t, int ln, char* str);
    // Parameters:
    // t -- number of type which label must be renamed
    // ln -- number of label that must be renamed
    // str -- string with the new name

char* GetLabelName(int t, int ln);

int SetTypeName(int t, char* str);
    // Parameters:
    // t -- number of type that must be renamed
    // str -- string with the new name
    // Returns:
    // 0 -- succes
    // 1 -- type with the given name is already exist

char* GetTypeName(int t);

int GetLabelNumber(int t, char* str);
    // Parameters:
    // t -- number of type which label number must be retrieved
    // str -- string with the label name
    // Returns:
    // -1 -- there is no label with given name
    // number of label that has the specified name

void ChangeVertexType(int n, int new_type);
    // Parameters:
    // n - number of vertex
    // new_type - new type of this object

void ChangeFragmentType(int n, int new_type);
void ChangeEdgeType(int n, int new_type);

void SimplifyGraph();
    // Removes all but one vertex types from the graph and
    // makes all vertices belong to one type (number 0).
    // Performs the same thing with fragments (type number 1)
    // and edges (type number 2).
    // Type parameters have initial values.
    // Doesn't change coordinates and sizes as well as edge bends.  

void UnfoldAll();

void RemoveAllBends(int par = 0);
    // Parameters:
    // par - if not zero, bends from loop edges are not removed 

void RenumV(int v1, int v2);
void RenumF(int f1, int f2);
void RenumE(int e1, int e2);

void NormalizeGraph(int red = 1);
    // Reduces sizes of fragments if red

// 10. Connection functions

void RegisterParameter(char* name, int* p);
void RegisterParameter(char* name, double* p);
void RegisterParameter(char* name, char* p);

int StartAlg(const char* lpCmdLine);
    // Returns:
    // 0 - success
    // 1 - users break
    // 2 - wrong number of parameters
    // 3 - wrong parameter
    // 4 - can't open first sample
    // 5 - can't load first sample

int EndStep();
    // Returns 1 if system wants e.p. to close immediately
    // otherwise returns 0

void EndAlg();

void SendMesLog(const char* mes);
int AskUserYN(const char* mes);
int AskUserOC(const char* mes);
void MesUser(const char* mes, int mes_type = 0);
    // mes_type :
    // 0 - information
    // 1 - warning
    // 2 - error
void AskUser(const char* mes, int& i);
void AskUser(const char* mes, double& f);
void AskUser(const char* mes, char* buf, int len = 256);
int AskVertexNumber(const char* mes);
int AskFragmentNumber(const char* mes);
int AskEdgeNumber(const char* mes);

// 11. Drawing interface functions

int StartDrawMethod(const char* lpCmdLine);
int EndDrawMethod(int is_success = 1);
void SetDrawCountPC(int pc);

// 12. String functions

int StrEq(const char* s1, const char* s2);
void StrCopy(const char* sfr, char* sto);
int StrLen(const char* str);
void StrConcat(char* beg, const char* end);

//--------------------

#define vertex(n) (Vertices->Acc(n))
#define fragment(n) (Fragments->Acc(n))
#define edge(n) (Edges->Acc(n))
#define label(n) Labels->Acc(n)
