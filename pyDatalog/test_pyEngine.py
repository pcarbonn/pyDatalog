from .pyEngine import *

if __name__ == "__main__":
    a = make_const('a')
    make_const('a')
    assert a.is_const() == True
    X = make_var('X')
    assert X.is_const() == False
    
    q = make_pred('q', 1)
    make_pred('q', 1)
    assert get_name(q) == 'q'
    assert get_arity(q) == '1'
    q2 = dup(q)
    
    # + q(a)
    qa = make_literal('q', [a])
    qa_ = make_clause(qa, [])
    assert_(qa_)
    
    # q(a, X)
    qaX = make_literal('q', [a, X])
    assert get_name(qaX.pred) == 'q'
    assert qaX.terms[0] == a
    assert a.get_id() =="ca"
    print(get_id(qaX))
    print(get_tag(qaX))
    print(get_id(rename(qaX)))
    
    print(ask2(qa, False))
    print(ask2(make_literal('q', [X]), True))
    print("Done.")