#include <iostream>
#include <vector>
#include "Node.h"

Node* Node::findChild(int c)
{
    //std::cout <<"8"<<std::endl;
    if(mChildren.size() == 0){
        return NULL;
    }

    //std::cout<<"9"<<std::endl;
    for ( int i = 0; i < mChildren.size(); i++ )
    {
        //std::cout<<"10"<<std::endl;
        Node* tmp = mChildren.at(i);
        if ( tmp->content() == c )
        {
            //std::cout <<"11"<<std::endl;
            return tmp;
        }
    }

    return NULL;
}

bool Node::deleteChild(int c)
{
    if(mChildren.size() == 0){
        return false;
    }

    for ( int i = 0; i < mChildren.size(); i++ )
    {
        Node* tmp = mChildren.at(i);
        if ( tmp->content() == c )
        {
            //delete child node
            mChildren.erase(mChildren.begin()+i);
            delete tmp;
            
            return true;
        }
    }

    return false;
}
