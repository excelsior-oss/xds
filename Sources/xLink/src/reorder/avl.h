
#ifndef _AVL_H_
#define _AVL_H_

#include <assert.h>

template<class Key, class Element> class AVLTree;


template<class Key, class Element> class TreeIterator {
  public:
    virtual void iter (Key k, Element e) = 0;
};

template<class Key, class Element> class AVLTreeNode {
  private:
    Key           key;
    Element       elem;
    AVLTreeNode * l;
    AVLTreeNode * r;
    int           bal;
    int           deleted;

    AVLTreeNode (Key k, Element e) :
        key     (k),
        elem    (e),
        l       (0),
        r       (0),
        bal     (0),
        deleted (0)
    {
    }

    static int Insert (Key k, Element e, AVLTreeNode **tree)
    {
        if ((*tree) == 0) {
            *tree = new AVLTreeNode (k, e);
            return 1;
        }

        AVLTreeNode * node = (*tree);
        if (node -> key > k)
        {
            if (Insert (k, e, &(node->l)))
            {
                switch (node->bal)
                {
                    case +1: node->bal =  0;  return 0;
                    case  0: node->bal = -1;  return 1;
                    case -1:
                    {
                        AVLTreeNode * left = node->l;
                        if (left->bal == -1) {
                            node -> l   = left -> r;
                            left -> r   = node;
                            node -> bal = 0;
                            (*tree)     = left;
                        } else {
                            AVLTreeNode * left_right = left -> r;
                            left -> r        = left_right -> l;
                            left_right -> l  = left;
                            node -> l        = left_right -> r;
                            left_right -> r  = node;
                            switch (left_right -> bal) {
                                case +1: left->bal=-1; node->bal= 0; break;
                                case -1: left->bal= 0; node->bal=+1; break;
                                case  0: left->bal= 0; node->bal= 0; break;
                            }
                            (*tree) = left_right;
                        }
                        (*tree)->bal = 0;
                        return 0;
                    }
                }
            } else
                return 0;
        } else if (node -> key < k) {
            if (Insert (k, e, &(node->r)))
            {
                switch (node->bal)
                {
                    case -1: node->bal =  0;  return 0;
                    case  0: node->bal = +1;  return 1;
                    case +1:
                    {
                        AVLTreeNode * right = node -> r;
                        if (right->bal == +1) {
                            node  -> r   = right->l;
                            right -> l   = node;
                            node  -> bal = 0;
                            (*tree)      = right;
                        } else {
                            AVLTreeNode * right_left = right -> l;
                            right      -> l = right_left->r;
                            right_left -> r = right;
                            node       -> r = right_left->l;
                            right_left -> l = node;
                            switch (right_left -> bal) {
                                case +1: right->bal= 0; node->bal=-1; break;
                                case -1: right->bal=+1; node->bal= 0; break;
                                case  0: right->bal= 0; node->bal= 0; break;
                            }
                            (*tree) = right_left;
                        }
                        (*tree)->bal = 0;
                        return 0;
                    }
                }
            } else
                return 0;
        } else {
            assert (0);
        }
        return 0;
    }

    friend void AVLTree<Key, Element>::Insert(Key k, Element e);

    AVLTreeNode * FindNode (Key k)
    {
        AVLTreeNode * node = this;
        while (node != 0) {
            if (k < node->key)
                node = node->l;
            else if (k > node->key)
                node = node->r;
            else
                return node -> deleted ? 0 : node;
        }
        return 0;
    }

  public:

    Element Find (Key k)
    {
        AVLTreeNode * node = FindNode (k);
        if (node != 0) {
            return node -> elem;
        }
        return 0;
    }

    Element FindAndRemove (Key k)
    {
        AVLTreeNode * node = FindNode (k);
        if (node != 0) {
            node -> deleted = 1;
            return node -> elem;
        }
        return 0;
    }

    int Height () {
        int l_height = (l == 0) ? 0 : l -> Height ();
        int r_height = (r == 0) ? 0 : r -> Height ();
        return ((l_height > r_height) ? l_height : r_height) + 1;
    }

    int Elements () {
        int l_elements = (l == 0) ? 0 : l -> Elements ();
        int r_elements = (r == 0) ? 0 : r -> Elements ();
        return l_elements + r_elements + (deleted ? 0 : 1);
    }

    void Iterate (TreeIterator<Key, Element> * i) {
        if (l != 0) {
            l -> Iterate (i);
        }
        if (!deleted) {
            i -> iter (key, elem);
        }
        if (r != 0) {
            r -> Iterate (i);
        }
    }

    ~AVLTreeNode () {
        if (l != 0) delete l;
        if (r != 0) delete r;
    }
};


template<class Key, class Element> class InsertTreeIterator : public TreeIterator<Key, Element> {
  private:
    AVLTree<Key, Element> * tree;

  public:
    InsertTreeIterator (AVLTree<Key, Element> * _tree) : tree (_tree) {}

    virtual void iter (Key k, Element e) {
        tree -> Insert (k, e);
    }
};

template<class Key, class Element> class AVLTree {
  private:
    AVLTreeNode<Key, Element> * tree;

  public:
    AVLTree () : tree (0) {}

    void Insert (Key k, Element e) {
        AVLTreeNode<Key, Element>::Insert (k, e, &tree);
    }

    Element FindAndRemove (Key k) {
        return (tree == 0) ? 0 : tree->FindAndRemove (k);
    }

    Element Find (Key k) {
        return (tree == 0) ? 0 : tree->Find (k);
    }

    int Height () {
        return (tree == 0) ? 0 : tree -> Height ();
    }

    int Elements () {
        return (tree == 0) ? 0 : tree -> Elements ();
    }

    void Iterate (TreeIterator<Key, Element> * i) {
        if (tree != 0)
            tree -> Iterate (i);
    }

    void Pack () {
        if (tree != 0) {
            AVLTreeNode<Key, Element> * oldtree = tree;
            tree = 0;
            InsertTreeIterator<Key, Element> i(this);
            oldtree->Iterate (&i);
            delete oldtree;
        }
    }

    ~AVLTree () {
        if (tree != 0) {
            delete tree;
        }
    }
};

#endif