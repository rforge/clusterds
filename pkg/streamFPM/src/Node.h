#include <iostream>
#include <vector>
#include <math.h>

class Node {
public:
    Node() { mContent = -1; mMarker = false; count=0;}
    ~Node() {}
    int content() { return mContent; }
    void setContent(int c) { mContent = c; }
    bool wordMarker() { return mMarker; }
    void setWordMarker() { mMarker = true; }
    void removeWordMarker() { mMarker = false; }
    void setCount(int cnt) { count = cnt; }
    int getCount() { return count; }
    int incrementCount(int add = 1){ count = count + add; }
    Node* findChild(int c);
    void appendChild(Node* child) { mChildren.push_back(child); }
    std::vector<Node*> children() { return mChildren; }
    void setErr(int e) { err = e; }
    int getErr() { return err; }
    void setId(int i) { id = i; }
    int getId() { return id; }
    
    bool deleteChild(int c);

private:
    int mContent;
    bool mMarker;
    int count;
    int err;
    int id;
    
    std::vector<Node*> mChildren;
};