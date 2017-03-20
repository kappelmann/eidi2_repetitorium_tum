open Ha8_angabe

(* Präsenzaufgabe 8.1 *)
let p1_a = [{ 
    assumption=BoolExpr (LinExpr (1, "a", 0), Eq, LinExpr (1, "b", 0)); 
    conclusion=BoolExpr (LinExpr (1, "a", 0), Eq, Constant 3) 
    }]
let p1_b = [{ 
    assumption=BoolExpr (Constant 17, Leq, LinExpr (1, "x", 0)); 
    conclusion=BoolExpr (LinExpr (1, "y", 0), Leq, LinExpr (1, "z", 0)) 
    }]
let p1_c = [{ 
    assumption=True; 
    conclusion=BoolExpr (LinExpr (1, "u", 0), Neq, LinExpr (1, "v", 0))
    }]
let p1_d = [{ 
    assumption=True; 
    conclusion=BoolExpr (LinExpr (1, "a", 0), Eq, LinExpr (1, "b", 0)) 
    }; { 
    assumption=True; 
    conclusion=BoolExpr (LinExpr (1, "c", 0), Le, LinExpr (1, "k", 0)) 
    }]
let p1_e = [{ 
    assumption=BoolExpr (LinExpr (1, "x", 4), Le, Constant 8); 
    conclusion=False; 
    }]
let p1_f = [{ 
    assumption=True; 
    conclusion=BoolExpr (LinExpr ((-5), "i", 8), Neq, LinExpr (1, "i", 0)) 
    }; { 
    assumption=True; 
    conclusion=BoolExpr (LinExpr (1, "g", 0), Neq, LinExpr (1, "n", 0)) 
    }]
let p1_g = [{ 
    assumption=BoolExpr (LinExpr (1, "u", 0), Leq, LinExpr (9, "h", (-1))); 
    conclusion=BoolExpr (Constant 9, Le, Constant 8) 
    }; { 
    assumption=BoolExpr (Constant 7, Eq, Constant 2);
    conclusion=BoolExpr (LinExpr (1, "u", 0), Leq, Constant 21) 
    }; { 
    assumption=BoolExpr (LinExpr (1, "t", 0), Eq, LinExpr (1, "r", 0)); 
    conclusion=BoolExpr (LinExpr (1, "b", 0), Neq, Constant 21) 
    }]
let p1_h = [{ 
    assumption=False; 
    conclusion=BoolExpr (LinExpr (1, "s", 0), Eq, Constant 42) 
    }]
let p1_i = [{ 
    assumption=True; 
    conclusion=False 
    }]
let p1_j = []

(* Präsenzaufgabe 8.2 *)
let p2_1 = 
    { 
        nodes = [ 
            (0, Start); 
            (1, Statement (Read "x"));
            (2, Stop);
        ];
        edges = [
            (0, None, None, 1);
            (1, None, None, 2);
        ]
    }

let p2_2 = 
    {
        nodes = [ 
            (0, Start);
            (1, Statement (Read "x"));
            (2, Statement (Assignment ("a", Constant 0)));
            (3, Statement (Write "a"));
            (4, Stop);
        ];
        edges = [
            (0, None, None, 1);
            (1, None, None, 2);
            (2, None, None, 3);
            (3, None, Some [{ assumption=True; conclusion=BoolExpr (LinExpr (1, "a", 0), Eq, Constant 0) }], 4);
        ]
    }

let p2_3 = 
    {
        nodes = [
            (0, Start);
            (1, Statement (Assignment ("a", Constant 4)));
            (2, Branch (BoolExpr (LinExpr (1, "a", 0), Neq, Constant 0)));
            (3, Statement (Assignment ("a", LinExpr ((-1), "a", 7))));
            (4, Join);
            (5, Stop);
        ];
        edges = [
            (0, None, None, 1);
            (1, None, None, 2);
            (2, Some No, None, 4);
            (2, Some Yes, None, 3);
            (3, None, None, 4);
            (4, None, Some [{ assumption=True; conclusion=BoolExpr (LinExpr (1, "a", 0), Leq, Constant 42); }], 5);
        ]
    }

let p2_4 = 
    {
        nodes = [
            (0, Start);
            (1, Statement (Assignment ("x", Constant 0)));
            (2, Statement (Read "y"));
            (3, Branch (BoolExpr (LinExpr (1, "y", 0), Leq, LinExpr (1, "x", 0))));
            (4, Statement (Assignment ("k", LinExpr (7, "y", 1))));
            (5, Statement (Assignment ("x", LinExpr (1, "y", (-4)))));
            (6, Statement (Assignment ("y", LinExpr (2, "x", (-6)))));
            (7, Statement (Assignment ("k", LinExpr ((-1), "y", (-4)))));
            (8, Branch (BoolExpr (LinExpr (1, "k", 0), Eq, LinExpr (1, "x", 2))));
            (9, Statement (Assignment ("y", LinExpr ((-2), "k", 0))));
            (10, Statement (Assignment ("x", LinExpr (21, "k", 5))));
            (11, Statement (Write "k"));
            (12, Statement (Assignment ("x", LinExpr ((-1), "x", 1))));
            (13, Statement (Read "v"));
            (14, Join);
            (15, Statement (Assignment ("k", LinExpr (17, "v", (-8)))));
            (16, Join);
            (17, Statement (Write "k"));
            (18, Statement (Write ("y")));
            (19, Stop);
        ];
        edges = [
            (0, None, None, 1);
            (1, None, None, 2);
            (2, None, None, 3);
            (3, Some No, None, 4);
            (4, None, None, 5);
            (5, None, None, 6);
            (6, None, Some [{ 
                assumption=BoolExpr (LinExpr (1, "x", 0), Neq, Constant 32);
                conclusion=BoolExpr (LinExpr (7, "y", 0), Leq, LinExpr (1, "k", 0)); 
                }; { 
                assumption=True; 
                conclusion=BoolExpr (LinExpr (1, "x", 0), Eq, Constant 0); 
                }], 16);
            (3, Some Yes, None, 7);
            (7, None, None, 8);
            (8, Some No, None, 9);
            (9, None, None, 10);
            (10, None, None, 14);
            (8, Some Yes, None, 11);
            (11, None, None, 12);
            (12, None, Some [{ 
                assumption=True; 
                conclusion=BoolExpr (LinExpr (1, "x", 0), Leq, LinExpr (1, "k", 0)); 
                }; { 
                assumption=True; 
                conclusion=BoolExpr (LinExpr (1, "x", 0), Neq, LinExpr (1, "y", 0)); 
                }], 13);
            (13, None, None, 14);
            (14, None, None, 15);
            (15, None, None, 16);
            (16, None, None, 17);
            (17, None, None, 18);
            (18, None, Some [{ 
                assumption=True; 
                conclusion=BoolExpr (LinExpr (1, "k", 0), Neq, LinExpr (1, "y", 0)); 
                }; { 
                assumption=True; 
                conclusion=BoolExpr (Constant 0, Le, LinExpr (1, "x", 0)); 
                }], 19);
        ]
    }