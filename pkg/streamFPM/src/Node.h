#include <iostream>
#include <vector>
#include <math.h>

class Node {
public:
    Node() { mContent = -1; mMarker = false; count=0;}
    ~Node() {}
    int content() { return mContent; }
    void setContent(int c) { mContent = c; }

    void setCount(double cnt) { count = cnt; }
    double getCount() { return count; }
    void incrementCount(double add = 1){ count = count + add; }
    
    Node* findChild(int c);
    void appendChild(Node* child) { mChildren.push_back(child); }
    std::vector<Node*> children() { return mChildren; }
    void setErr(double e) { err = e; }
    double getErr() { return err; }
    void setId(int i) { id = i; }
    int getId() { return id; }
    
    bool removeChild(int c);
    
    //not used
    bool wordMarker() { return mMarker; }
    void setWordMarker() { mMarker = true; }
    void removeWordMarker() { mMarker = false; }
    //
    


private:
    //mContent is the item that the node is tracking
    int mContent;
    bool mMarker;
    double count;
    double err;
    //id = TID last transaction that had this item
    int id;
    
    std::vector<Node*> mChildren;
};