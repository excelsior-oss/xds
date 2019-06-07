
#ifndef _LIST_H_
#define _LIST_H_

template <class Element> class List;

template <class Element> class ListElement {
  private:
    Element       elem;
    ListElement * next;

    ListElement (Element e) : elem (e), next (0) {}

    friend class List<Element>;
};


template <class Element> class ListIterator {
  public:
    virtual void iter (Element e) = 0;
};


template <class Element> class List {
  private:
    ListElement<Element> * first;
    ListElement<Element> * last;

  public:
    List () : first (0), last (0) {}

    void Add (Element e) {
        ListElement<Element> * le = new ListElement<Element> (e);
        if (first != 0) {
            assert (last != 0);
            last -> next = le;
        } else {
            first = le;
        }
        last = le;
    }

    void Iterate (ListIterator<Element> * i) {
        for (ListElement<Element> * cur = first; cur; cur = cur -> next) {
            i -> iter (cur -> elem);
        }
     }
};

#endif _LIST_H_
