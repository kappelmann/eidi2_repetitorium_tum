open Kaputt.Abbreviations
open Ha6_angabe
open Batteries.List

module X = Ha6_sol
module Sol = Ha6_sol

let tests = ref []
let points = ref []
let add points_test test =
  points := !points @ [points_test];
  tests := !tests @ [test]

let valid_avls = [
  ([13; 15; 29; -3; 8; 19; 3; 25; 22; 0],
   (Ha6_angabe.Node
      { Ha6_angabe.key = 15; balance = 0;
        left =
          (Ha6_angabe.Node
             { Ha6_angabe.key = 8; balance = -1;
               left =
                 (Ha6_angabe.Node
                    { Ha6_angabe.key = 0; balance = 0;
                      left =
                        (Ha6_angabe.Node
                           { Ha6_angabe.key = -3; balance = 0; left = Ha6_angabe.Leaf;
                             right = Ha6_angabe.Leaf });
                      right =
                        (Ha6_angabe.Node
                           { Ha6_angabe.key = 3; balance = 0; left = Ha6_angabe.Leaf;
                             right = Ha6_angabe.Leaf })
                    });
               right =
                 (Ha6_angabe.Node
                    { Ha6_angabe.key = 13; balance = 0; left = Ha6_angabe.Leaf;
                      right = Ha6_angabe.Leaf })
             });
        right =
          (Ha6_angabe.Node
             { Ha6_angabe.key = 25; balance = -1;
               left =
                 (Ha6_angabe.Node
                    { Ha6_angabe.key = 19; balance = 1; left = Ha6_angabe.Leaf;
                      right =
                        (Ha6_angabe.Node
                           { Ha6_angabe.key = 22; balance = 0; left = Ha6_angabe.Leaf;
                             right = Ha6_angabe.Leaf })
                    });
               right =
                 (Ha6_angabe.Node
                    { Ha6_angabe.key = 29; balance = 0; left = Ha6_angabe.Leaf;
                      right = Ha6_angabe.Leaf })
             })
      }));
  ([29; 6; -1; 34; 25; 44; 37; 1; -2; 50; 60; 46; 9; 0; 39; 55; 12; 13; 23; 18],
   (Ha6_angabe.Node
      { Ha6_angabe.key = 29; balance = 0;
        left =
          (Ha6_angabe.Node
             { Ha6_angabe.key = 6; balance = 0;
               left =
                 (Ha6_angabe.Node
                    { Ha6_angabe.key = -1; balance = 1;
                      left =
                        (Ha6_angabe.Node
                           { Ha6_angabe.key = -2; balance = 0; left = Ha6_angabe.Leaf;
                             right = Ha6_angabe.Leaf });
                      right =
                        (Ha6_angabe.Node
                           { Ha6_angabe.key = 1; balance = -1;
                             left =
                               (Ha6_angabe.Node
                                  { Ha6_angabe.key = 0; balance = 0;
                                    left = Ha6_angabe.Leaf; right = Ha6_angabe.Leaf });
                             right = Ha6_angabe.Leaf })
                    });
               right =
                 (Ha6_angabe.Node
                    { Ha6_angabe.key = 13; balance = 0;
                      left =
                        (Ha6_angabe.Node
                           { Ha6_angabe.key = 12; balance = -1;
                             left =
                               (Ha6_angabe.Node
                                  { Ha6_angabe.key = 9; balance = 0;
                                    left = Ha6_angabe.Leaf; right = Ha6_angabe.Leaf });
                             right = Ha6_angabe.Leaf });
                      right =
                        (Ha6_angabe.Node
                           { Ha6_angabe.key = 23; balance = 0;
                             left =
                               (Ha6_angabe.Node
                                  { Ha6_angabe.key = 18; balance = 0;
                                    left = Ha6_angabe.Leaf; right = Ha6_angabe.Leaf });
                             right =
                               (Ha6_angabe.Node
                                  { Ha6_angabe.key = 25; balance = 0;
                                    left = Ha6_angabe.Leaf; right = Ha6_angabe.Leaf })
                           })
                    })
             });
        right =
          (Ha6_angabe.Node
             { Ha6_angabe.key = 44; balance = 1;
               left =
                 (Ha6_angabe.Node
                    { Ha6_angabe.key = 37; balance = 0;
                      left =
                        (Ha6_angabe.Node
                           { Ha6_angabe.key = 34; balance = 0; left = Ha6_angabe.Leaf;
                             right = Ha6_angabe.Leaf });
                      right =
                        (Ha6_angabe.Node
                           { Ha6_angabe.key = 39; balance = 0; left = Ha6_angabe.Leaf;
                             right = Ha6_angabe.Leaf })
                    });
               right =
                 (Ha6_angabe.Node
                    { Ha6_angabe.key = 50; balance = 1;
                      left =
                        (Ha6_angabe.Node
                           { Ha6_angabe.key = 46; balance = 0; left = Ha6_angabe.Leaf;
                             right = Ha6_angabe.Leaf });
                      right =
                        (Ha6_angabe.Node
                           { Ha6_angabe.key = 60; balance = -1;
                             left =
                               (Ha6_angabe.Node
                                  { Ha6_angabe.key = 55; balance = 0;
                                    left = Ha6_angabe.Leaf; right = Ha6_angabe.Leaf });
                             right = Ha6_angabe.Leaf })
                    })
             })
      }))
]

let invalid_avls = [
  ([9; 36; 16; -1; 4; 17; 31; 27; 22; 13],
   (Ha6_angabe.Node
      { Ha6_angabe.key = 16; balance = 0;
        left =
          (Ha6_angabe.Node
             { Ha6_angabe.key = 4; balance = -1;
               left =
                 (Ha6_angabe.Node
                    { Ha6_angabe.key = -1; balance = 0; left = Ha6_angabe.Leaf;
                      right = Ha6_angabe.Leaf });
               right =
                 (Ha6_angabe.Node
                    { Ha6_angabe.key = 9; balance = 1; left = Ha6_angabe.Leaf;
                      right =
                        (Ha6_angabe.Node
                           { Ha6_angabe.key = 13; balance = 0; left = Ha6_angabe.Leaf;
                             right = Ha6_angabe.Leaf })
                    })
             });
        right =
          (Ha6_angabe.Node
             { Ha6_angabe.key = 31; balance = -1;
               left =
                 (Ha6_angabe.Node
                    { Ha6_angabe.key = 22; balance = 0;
                      left =
                        (Ha6_angabe.Node
                           { Ha6_angabe.key = 17; balance = 0; left = Ha6_angabe.Leaf;
                             right = Ha6_angabe.Leaf });
                      right =
                        (Ha6_angabe.Node
                           { Ha6_angabe.key = 27; balance = 0; left = Ha6_angabe.Leaf;
                             right = Ha6_angabe.Leaf })
                    });
               right =
                 (Ha6_angabe.Node
                    { Ha6_angabe.key = 36; balance = 0; left = Ha6_angabe.Leaf;
                      right = Ha6_angabe.Leaf })
             })
      }));
  ([20; 22; 15; 0; 27; 12; 1; 2; 3; 7],
   (Ha6_angabe.Node
      { Ha6_angabe.key = 12; balance = 0;
        left =
          (Ha6_angabe.Node
             { Ha6_angabe.key = 2; balance = 0;
               left =
                 (Ha6_angabe.Node
                    { Ha6_angabe.key = 1; balance = -1;
                      left =
                        (Ha6_angabe.Node
                           { Ha6_angabe.key = 0; balance = 0; left = Ha6_angabe.Leaf;
                             right = Ha6_angabe.Leaf });
                      right = Ha6_angabe.Leaf });
               right =
                 (Ha6_angabe.Node
                    { Ha6_angabe.key = 3; balance = 1; left = Ha6_angabe.Leaf;
                      right =
                        (Ha6_angabe.Node
                           { Ha6_angabe.key = 7; balance = 0; left = Ha6_angabe.Leaf;
                             right = Ha6_angabe.Leaf })
                    })
             });
        right =
          (Ha6_angabe.Node
             { Ha6_angabe.key = 20; balance = 1;
               left =
                 (Ha6_angabe.Node
                    { Ha6_angabe.key = 15; balance = 0; left = Ha6_angabe.Leaf;
                      right = Ha6_angabe.Leaf });
               right =
                 (Ha6_angabe.Node
                    { Ha6_angabe.key = 22; balance = 2; left = Ha6_angabe.Leaf;
                      right =
                        (Ha6_angabe.Node
                           { Ha6_angabe.key = 27; balance = 0; left = Ha6_angabe.Leaf;
                             right = Ha6_angabe.Leaf })
                    })
             })
      }));
  ([19; 16; 5; -1; 22; 23; 7; 20; 12; 4],
   (Ha6_angabe.Node
      { Ha6_angabe.key = 16; balance = 0;
        left =
          (Ha6_angabe.Node
             { Ha6_angabe.key = 5; balance = 0;
               left =
                 (Ha6_angabe.Node
                    { Ha6_angabe.key = -1; balance = 1; left = Ha6_angabe.Leaf;
                      right =
                        (Ha6_angabe.Node
                           { Ha6_angabe.key = 6; balance = 0; left = Ha6_angabe.Leaf;
                             right = Ha6_angabe.Leaf })
                    });
               right =
                 (Ha6_angabe.Node
                    { Ha6_angabe.key = 7; balance = 1; left = Ha6_angabe.Leaf;
                      right =
                        (Ha6_angabe.Node
                           { Ha6_angabe.key = 12; balance = 0; left = Ha6_angabe.Leaf;
                             right = Ha6_angabe.Leaf })
                    })
             });
        right =
          (Ha6_angabe.Node
             { Ha6_angabe.key = 22; balance = -1;
               left =
                 (Ha6_angabe.Node
                    { Ha6_angabe.key = 19; balance = 1; left = Ha6_angabe.Leaf;
                      right =
                        (Ha6_angabe.Node
                           { Ha6_angabe.key = 20; balance = 0; left = Ha6_angabe.Leaf;
                             right = Ha6_angabe.Leaf })
                    });
               right =
                 (Ha6_angabe.Node
                    { Ha6_angabe.key = 23; balance = 0; left = Ha6_angabe.Leaf;
                      right = Ha6_angabe.Leaf })
             })
      }));
  ([-2; -4; 16; 11; 4; 22; 19; 13; -1; 6],
   (Ha6_angabe.Node
      { Ha6_angabe.key = 11; balance = 0;
        left =
          (Ha6_angabe.Node
             { Ha6_angabe.key = -2; balance = 1;
               left =
                 (Ha6_angabe.Node
                    { Ha6_angabe.key = -4; balance = 0; left = Ha6_angabe.Leaf;
                      right = Ha6_angabe.Leaf });
               right =
                 (Ha6_angabe.Node
                    { Ha6_angabe.key = 4; balance = 0;
                      left =
                        (Ha6_angabe.Node
                           { Ha6_angabe.key = -1; balance = 0; left = Ha6_angabe.Leaf;
                             right = Ha6_angabe.Leaf });
                      right =
                        (Ha6_angabe.Node
                           { Ha6_angabe.key = 6; balance = 0; left = Ha6_angabe.Leaf;
                             right = Ha6_angabe.Leaf })
                    })
             });
        right =
          (Ha6_angabe.Node
             { Ha6_angabe.key = 19; balance = -1;
               left =
                 (Ha6_angabe.Node
                    { Ha6_angabe.key = 16; balance = -1;
                      left =
                        (Ha6_angabe.Node
                           { Ha6_angabe.key = 13; balance = 0; left = Ha6_angabe.Leaf;
                             right = Ha6_angabe.Leaf });
                      right = Ha6_angabe.Leaf });
               right =
                 (Ha6_angabe.Node
                    { Ha6_angabe.key = -5; balance = 0; left = Ha6_angabe.Leaf;
                      right = Ha6_angabe.Leaf })
             })
      }));
  ([8; 2; -3; 4; 0; 3; 5],
   (Ha6_angabe.Node
      { Ha6_angabe.key = 8; balance = -1;
        left =
          (Ha6_angabe.Node
             { Ha6_angabe.key = 2; balance = 0;
               left =
                 (Ha6_angabe.Node
                    { Ha6_angabe.key = -3; balance = 1; left = Ha6_angabe.Leaf;
                      right =
                        (Ha6_angabe.Node
                           { Ha6_angabe.key = 0; balance = 0; left = Ha6_angabe.Leaf;
                             right = Ha6_angabe.Leaf })
                    });
               right =
                 (Ha6_angabe.Node
                    { Ha6_angabe.key = 4; balance = 0;
                      left =
                        (Ha6_angabe.Node
                           { Ha6_angabe.key = 3; balance = 0; left = Ha6_angabe.Leaf;
                             right = Ha6_angabe.Leaf });
                      right =
                        (Ha6_angabe.Node
                           { Ha6_angabe.key = 5; balance = 0; left = Ha6_angabe.Leaf;
                             right = Ha6_angabe.Leaf })
                    })
             });
        right = Ha6_angabe.Leaf
      })
  )
]

let test_valid_avl () =
  List.iter (fun (_, t) -> Assert.is_true ~msg:"Valid tree considered invalid" (X.valid_avl t)) valid_avls;
  List.iter (fun (_, t) -> Assert.is_false ~msg:"Invalid tree considered valid" (X.valid_avl t)) invalid_avls

let test_single_rotate () =
  let single_rots = [
    (
      Ha6_angabe.Left, 
      (Ha6_angabe.Node
         { Ha6_angabe.key = 36; balance = 2; left = Ha6_angabe.Leaf;
           right =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 116; balance = 1; left = Ha6_angabe.Leaf;
                  right =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 180; balance = 0; left = Ha6_angabe.Leaf;
                         right = Ha6_angabe.Leaf })
                })
         }), 
      (Ha6_angabe.Node
         { Ha6_angabe.key = 116; balance = 0;
           left =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 36; balance = 0; left = Ha6_angabe.Leaf;
                  right = Ha6_angabe.Leaf });
           right =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 180; balance = 0; left = Ha6_angabe.Leaf;
                  right = Ha6_angabe.Leaf })
         })
    );
    (
      Ha6_angabe.Right, 
      (Ha6_angabe.Node
         { Ha6_angabe.key = 276; balance = -2;
           left =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 205; balance = -1;
                  left =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 180; balance = -1;
                         left =
                           (Ha6_angabe.Node
                              { Ha6_angabe.key = 155; balance = 0; left = Ha6_angabe.Leaf;
                                right = Ha6_angabe.Leaf });
                         right = Ha6_angabe.Leaf });
                  right =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 209; balance = 0; left = Ha6_angabe.Leaf;
                         right = Ha6_angabe.Leaf })
                });
           right =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 305; balance = 0; left = Ha6_angabe.Leaf;
                  right = Ha6_angabe.Leaf })
         }), 
      (Ha6_angabe.Node
         { Ha6_angabe.key = 205; balance = 0;
           left =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 180; balance = -1;
                  left =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 155; balance = 0; left = Ha6_angabe.Leaf;
                         right = Ha6_angabe.Leaf });
                  right = Ha6_angabe.Leaf });
           right =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 276; balance = 0;
                  left =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 209; balance = 0; left = Ha6_angabe.Leaf;
                         right = Ha6_angabe.Leaf });
                  right =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 305; balance = 0; left = Ha6_angabe.Leaf;
                         right = Ha6_angabe.Leaf })
                })
         })
    );
    (
      Ha6_angabe.Right, 
      (Ha6_angabe.Node
         { Ha6_angabe.key = 272; balance = -2;
           left =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 266; balance = -1;
                  left =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 244; balance = 0; left = Ha6_angabe.Leaf;
                         right = Ha6_angabe.Leaf });
                  right = Ha6_angabe.Leaf });
           right = Ha6_angabe.Leaf }), 
      (Ha6_angabe.Node
         { Ha6_angabe.key = 266; balance = 0;
           left =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 244; balance = 0; left = Ha6_angabe.Leaf;
                  right = Ha6_angabe.Leaf });
           right =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 272; balance = 0; left = Ha6_angabe.Leaf;
                  right = Ha6_angabe.Leaf })
         })
    );
    (
      Ha6_angabe.Left, 
      (Ha6_angabe.Node
         { Ha6_angabe.key = 25; balance = 2;
           left =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 14; balance = 1; left = Ha6_angabe.Leaf;
                  right =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 17; balance = 0; left = Ha6_angabe.Leaf;
                         right = Ha6_angabe.Leaf })
                });
           right =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 36; balance = 1;
                  left =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 34; balance = -1;
                         left =
                           (Ha6_angabe.Node
                              { Ha6_angabe.key = 29; balance = 0; left = Ha6_angabe.Leaf;
                                right = Ha6_angabe.Leaf });
                         right = Ha6_angabe.Leaf });
                  right =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 52; balance = -1;
                         left =
                           (Ha6_angabe.Node
                              { Ha6_angabe.key = 44; balance = -1;
                                left =
                                  (Ha6_angabe.Node
                                     { Ha6_angabe.key = 40; balance = 0;
                                       left = Ha6_angabe.Leaf; right = Ha6_angabe.Leaf });
                                right = Ha6_angabe.Leaf });
                         right =
                           (Ha6_angabe.Node
                              { Ha6_angabe.key = 60; balance = 0; left = Ha6_angabe.Leaf;
                                right = Ha6_angabe.Leaf })
                       })
                })
         }), 
      (Ha6_angabe.Node
         { Ha6_angabe.key = 36; balance = 0;
           left =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 25; balance = 0;
                  left =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 14; balance = 1; left = Ha6_angabe.Leaf;
                         right =
                           (Ha6_angabe.Node
                              { Ha6_angabe.key = 17; balance = 0; left = Ha6_angabe.Leaf;
                                right = Ha6_angabe.Leaf })
                       });
                  right =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 34; balance = -1;
                         left =
                           (Ha6_angabe.Node
                              { Ha6_angabe.key = 29; balance = 0; left = Ha6_angabe.Leaf;
                                right = Ha6_angabe.Leaf });
                         right = Ha6_angabe.Leaf })
                });
           right =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 52; balance = -1;
                  left =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 44; balance = -1;
                         left =
                           (Ha6_angabe.Node
                              { Ha6_angabe.key = 40; balance = 0; left = Ha6_angabe.Leaf;
                                right = Ha6_angabe.Leaf });
                         right = Ha6_angabe.Leaf });
                  right =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 60; balance = 0; left = Ha6_angabe.Leaf;
                         right = Ha6_angabe.Leaf })
                })
         })
    )
  ] in
  List.iter (fun (dir, before, after) -> Assert.equal (X.rotate_single dir before) after) single_rots

let test_double_rotate () =
  let double_rots = [
    (
      Ha6_angabe.Right, 
      (Ha6_angabe.Node
         { Ha6_angabe.key = 22; balance = -2;
           left =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 11; balance = 1; left = Ha6_angabe.Leaf;
                  right =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 18; balance = 0; left = Ha6_angabe.Leaf;
                         right = Ha6_angabe.Leaf })
                });
           right = Ha6_angabe.Leaf }), 
      (Ha6_angabe.Node
         { Ha6_angabe.key = 18; balance = 0;
           left =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 11; balance = 0; left = Ha6_angabe.Leaf;
                  right = Ha6_angabe.Leaf });
           right =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 22; balance = 0; left = Ha6_angabe.Leaf;
                  right = Ha6_angabe.Leaf })
         })
    );
    (
      Ha6_angabe.Right, 
      (Ha6_angabe.Node
         { Ha6_angabe.key = 132; balance = -2;
           left =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 93; balance = 1;
                  left =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 78; balance = 1; left = Ha6_angabe.Leaf;
                         right =
                           (Ha6_angabe.Node
                              { Ha6_angabe.key = 83; balance = 0; left = Ha6_angabe.Leaf;
                                right = Ha6_angabe.Leaf })
                       });
                  right =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 112; balance = 1;
                         left =
                           (Ha6_angabe.Node
                              { Ha6_angabe.key = 97; balance = 0; left = Ha6_angabe.Leaf;
                                right = Ha6_angabe.Leaf });
                         right =
                           (Ha6_angabe.Node
                              { Ha6_angabe.key = 114; balance = 1; left = Ha6_angabe.Leaf;
                                right =
                                  (Ha6_angabe.Node
                                     { Ha6_angabe.key = 122; balance = 0;
                                       left = Ha6_angabe.Leaf; right = Ha6_angabe.Leaf })
                              })
                       })
                });
           right =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 143; balance = 0;
                  left =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 137; balance = 0; left = Ha6_angabe.Leaf;
                         right = Ha6_angabe.Leaf });
                  right =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 148; balance = 0; left = Ha6_angabe.Leaf;
                         right = Ha6_angabe.Leaf })
                })
         }), 
      (Ha6_angabe.Node
         { Ha6_angabe.key = 112; balance = 0;
           left =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 93; balance = -1;
                  left =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 78; balance = 1; left = Ha6_angabe.Leaf;
                         right =
                           (Ha6_angabe.Node
                              { Ha6_angabe.key = 83; balance = 0; left = Ha6_angabe.Leaf;
                                right = Ha6_angabe.Leaf })
                       });
                  right =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 97; balance = 0; left = Ha6_angabe.Leaf;
                         right = Ha6_angabe.Leaf })
                });
           right =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 132; balance = 0;
                  left =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 114; balance = 1; left = Ha6_angabe.Leaf;
                         right =
                           (Ha6_angabe.Node
                              { Ha6_angabe.key = 122; balance = 0; left = Ha6_angabe.Leaf;
                                right = Ha6_angabe.Leaf })
                       });
                  right =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 143; balance = 0;
                         left =
                           (Ha6_angabe.Node
                              { Ha6_angabe.key = 137; balance = 0; left = Ha6_angabe.Leaf;
                                right = Ha6_angabe.Leaf });
                         right =
                           (Ha6_angabe.Node
                              { Ha6_angabe.key = 148; balance = 0; left = Ha6_angabe.Leaf;
                                right = Ha6_angabe.Leaf })
                       })
                })
         })
    );
    (
      Ha6_angabe.Left, 
      (Ha6_angabe.Node
         { Ha6_angabe.key = 22; balance = 2; left = Ha6_angabe.Leaf;
           right =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 33; balance = -1;
                  left =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 25; balance = 0; left = Ha6_angabe.Leaf;
                         right = Ha6_angabe.Leaf });
                  right = Ha6_angabe.Leaf })
         }), 
      (Ha6_angabe.Node
         { Ha6_angabe.key = 25; balance = 0;
           left =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 22; balance = 0; left = Ha6_angabe.Leaf;
                  right = Ha6_angabe.Leaf });
           right =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 33; balance = 0; left = Ha6_angabe.Leaf;
                  right = Ha6_angabe.Leaf })
         })
    );
    (
      Ha6_angabe.Left, 
      (Ha6_angabe.Node
         { Ha6_angabe.key = 18; balance = 2;
           left =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 11; balance = -1;
                  left =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = -1; balance = 0; left = Ha6_angabe.Leaf;
                         right = Ha6_angabe.Leaf });
                  right = Ha6_angabe.Leaf });
           right =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 52; balance = -1;
                  left =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 25; balance = 1;
                         left =
                           (Ha6_angabe.Node
                              { Ha6_angabe.key = 22; balance = 0; left = Ha6_angabe.Leaf;
                                right = Ha6_angabe.Leaf });
                         right =
                           (Ha6_angabe.Node
                              { Ha6_angabe.key = 33; balance = 1; left = Ha6_angabe.Leaf;
                                right =
                                  (Ha6_angabe.Node
                                     { Ha6_angabe.key = 48; balance = 0;
                                       left = Ha6_angabe.Leaf; right = Ha6_angabe.Leaf })
                              })
                       });
                  right =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 67; balance = 0;
                         left =
                           (Ha6_angabe.Node
                              { Ha6_angabe.key = 64; balance = 0; left = Ha6_angabe.Leaf;
                                right = Ha6_angabe.Leaf });
                         right =
                           (Ha6_angabe.Node
                              { Ha6_angabe.key = 72; balance = 0; left = Ha6_angabe.Leaf;
                                right = Ha6_angabe.Leaf })
                       })
                })
         }), 
      (Ha6_angabe.Node
         { Ha6_angabe.key = 25; balance = 0;
           left =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 18; balance = -1;
                  left =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 11; balance = -1;
                         left =
                           (Ha6_angabe.Node
                              { Ha6_angabe.key = -1; balance = 0; left = Ha6_angabe.Leaf;
                                right = Ha6_angabe.Leaf });
                         right = Ha6_angabe.Leaf });
                  right =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 22; balance = 0; left = Ha6_angabe.Leaf;
                         right = Ha6_angabe.Leaf })
                });
           right =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 52; balance = 0;
                  left =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 33; balance = 1; left = Ha6_angabe.Leaf;
                         right =
                           (Ha6_angabe.Node
                              { Ha6_angabe.key = 48; balance = 0; left = Ha6_angabe.Leaf;
                                right = Ha6_angabe.Leaf })
                       });
                  right =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 67; balance = 0;
                         left =
                           (Ha6_angabe.Node
                              { Ha6_angabe.key = 64; balance = 0; left = Ha6_angabe.Leaf;
                                right = Ha6_angabe.Leaf });
                         right =
                           (Ha6_angabe.Node
                              { Ha6_angabe.key = 72; balance = 0; left = Ha6_angabe.Leaf;
                                right = Ha6_angabe.Leaf })
                       })
                })
         }) 
    )
  ] in
  List.iter (fun (dir, before, after) -> Assert.equal (X.rotate_double dir before) after) double_rots

let test_rebalance () = 
  let rots = [
    (
      (Ha6_angabe.Node
         { Ha6_angabe.key = 277; balance = 1; left = Ha6_angabe.Leaf;
           right =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 294; balance = 0; left = Ha6_angabe.Leaf;
                  right = Ha6_angabe.Leaf })
         }), 
      (Ha6_angabe.Node
         { Ha6_angabe.key = 277; balance = 1; left = Ha6_angabe.Leaf;
           right =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 294; balance = 0; left = Ha6_angabe.Leaf;
                  right = Ha6_angabe.Leaf })
         })

    );
    (
      (Ha6_angabe.Node
         { Ha6_angabe.key = 277; balance = 0;
           left =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 201; balance = 0; left = Ha6_angabe.Leaf;
                  right = Ha6_angabe.Leaf });
           right =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 294; balance = 0; left = Ha6_angabe.Leaf;
                  right = Ha6_angabe.Leaf })
         }), 
      (Ha6_angabe.Node
         { Ha6_angabe.key = 277; balance = 0;
           left =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 201; balance = 0; left = Ha6_angabe.Leaf;
                  right = Ha6_angabe.Leaf });
           right =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 294; balance = 0; left = Ha6_angabe.Leaf;
                  right = Ha6_angabe.Leaf })
         })

    );
    (
      (Ha6_angabe.Node
         { Ha6_angabe.key = 201; balance = -2;
           left =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 9; balance = 1; left = Ha6_angabe.Leaf;
                  right =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 84; balance = 0; left = Ha6_angabe.Leaf;
                         right = Ha6_angabe.Leaf })
                });
           right = Ha6_angabe.Leaf }), 
      (Ha6_angabe.Node
         { Ha6_angabe.key = 84; balance = 0;
           left =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 9; balance = 0; left = Ha6_angabe.Leaf;
                  right = Ha6_angabe.Leaf });
           right =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 201; balance = 0; left = Ha6_angabe.Leaf;
                  right = Ha6_angabe.Leaf })
         })
    );
    (
      (Ha6_angabe.Node
         { Ha6_angabe.key = 277; balance = -2;
           left =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 84; balance = -1;
                  left =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 9; balance = 1; left = Ha6_angabe.Leaf;
                         right =
                           (Ha6_angabe.Node
                              { Ha6_angabe.key = 65; balance = 0; left = Ha6_angabe.Leaf;
                                right = Ha6_angabe.Leaf })
                       });
                  right =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 201; balance = 0; left = Ha6_angabe.Leaf;
                         right = Ha6_angabe.Leaf })
                });
           right =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 294; balance = 0; left = Ha6_angabe.Leaf;
                  right = Ha6_angabe.Leaf })
         }), 
      (Ha6_angabe.Node
         { Ha6_angabe.key = 84; balance = 0;
           left =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 9; balance = 1; left = Ha6_angabe.Leaf;
                  right =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 65; balance = 0; left = Ha6_angabe.Leaf;
                         right = Ha6_angabe.Leaf })
                });
           right =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 277; balance = 0;
                  left =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 201; balance = 0; left = Ha6_angabe.Leaf;
                         right = Ha6_angabe.Leaf });
                  right =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 294; balance = 0; left = Ha6_angabe.Leaf;
                         right = Ha6_angabe.Leaf })
                })
         })
    );
    (
      (Ha6_angabe.Node
         { Ha6_angabe.key = 201; balance = -2;
           left =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 177; balance = -1;
                  left =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 103; balance = 0; left = Ha6_angabe.Leaf;
                         right = Ha6_angabe.Leaf });
                  right = Ha6_angabe.Leaf });
           right = Ha6_angabe.Leaf }), 
      (Ha6_angabe.Node
         { Ha6_angabe.key = 177; balance = 0;
           left =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 103; balance = 0; left = Ha6_angabe.Leaf;
                  right = Ha6_angabe.Leaf });
           right =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 201; balance = 0; left = Ha6_angabe.Leaf;
                  right = Ha6_angabe.Leaf })
         })
    );
    (
      (Ha6_angabe.Node
         { Ha6_angabe.key = 277; balance = -2;
           left =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 177; balance = 1;
                  left =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 103; balance = 0; left = Ha6_angabe.Leaf;
                         right = Ha6_angabe.Leaf });
                  right =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 201; balance = -1;
                         left =
                           (Ha6_angabe.Node
                              { Ha6_angabe.key = 182; balance = 0; left = Ha6_angabe.Leaf;
                                right = Ha6_angabe.Leaf });
                         right = Ha6_angabe.Leaf })
                });
           right =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 294; balance = 0; left = Ha6_angabe.Leaf;
                  right = Ha6_angabe.Leaf })
         }), 
      (Ha6_angabe.Node
         { Ha6_angabe.key = 201; balance = 0;
           left =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 177; balance = 0;
                  left =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 103; balance = 0; left = Ha6_angabe.Leaf;
                         right = Ha6_angabe.Leaf });
                  right =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 182; balance = 0; left = Ha6_angabe.Leaf;
                         right = Ha6_angabe.Leaf })
                });
           right =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 277; balance = 1; left = Ha6_angabe.Leaf;
                  right =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 294; balance = 0; left = Ha6_angabe.Leaf;
                         right = Ha6_angabe.Leaf })
                })
         })
    );
    (
      (Ha6_angabe.Node
         { Ha6_angabe.key = 9; balance = 2; left = Ha6_angabe.Leaf;
           right =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 65; balance = -1;
                  left =
                    (Ha6_angabe.Node
                       { Ha6_angabe.key = 54; balance = 0; left = Ha6_angabe.Leaf;
                         right = Ha6_angabe.Leaf });
                  right = Ha6_angabe.Leaf })
         }), 
      (Ha6_angabe.Node
         { Ha6_angabe.key = 54; balance = 0;
           left =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 9; balance = 0; left = Ha6_angabe.Leaf;
                  right = Ha6_angabe.Leaf });
           right =
             (Ha6_angabe.Node
                { Ha6_angabe.key = 65; balance = 0; left = Ha6_angabe.Leaf;
                  right = Ha6_angabe.Leaf })
         })
    );
  ] in
  List.iter (fun (before, after) -> Assert.equal (X.rebalance before) after) rots

let test_print_dot () =
  Assert.fail_msg "Your submission for this function needs to be corrected by your tutor."

let test_insert () =
  let shuffle d =
    let nd = List.rev_map (fun c -> (Random.bits (), c)) d in
    let sond = List.sort (fun (x, _) (y, _) -> Pervasives.compare x y) nd in
    List.rev_map snd sond in
  let random_list_no_duplicates length =
    let last = ref (-100) in
    let l = init length (fun _ ->
        let next = !last + 1 + (Random.int 5) in
        last := next;
        next) in
    let l_shuff = shuffle l in
    (l, l_shuff) in
  let rec to_sorted_list = function
      Node {key; balance; left; right} ->
      (to_sorted_list left) @ [key] @ (to_sorted_list right)
    | Leaf -> [] in
  for i = 10 to 15 do
    let count = i + (i - 10)*1000 in
    let (list_sorted, list_shuffled) = random_list_no_duplicates count in
    let t = fold_left (fun t x -> X.insert x t) Leaf list_shuffled in
    Assert.is_true ~msg:"AVL tree not valid" (Ha6_sol.valid_avl t); 
    let tree_sorted = to_sorted_list t in
    Assert.is_true ~msg:"Elements lost or new elements appeared" (tree_sorted = list_sorted)
  done

let () =
  add 3 @@ Test.make_simple_test ~title:"test_valid_avl" test_valid_avl;
  add 4 @@ Test.make_simple_test ~title:"test_single_rotate" test_single_rotate;
  add 4 @@ Test.make_simple_test ~title:"test_double_rotate" test_double_rotate;
  add 3 @@ Test.make_simple_test ~title:"test_rebalance" test_rebalance;
  add 3 @@ Test.make_simple_test ~title:"test_insert" test_insert;
  add 3 @@ Test.make_simple_test ~title:"test_print_dot" test_print_dot

(* launch *)
let () =
  Random.self_init ();
  let point = Test.(function
      | Passed -> 1
      | Report (p,n,e,c,m) when p = n -> 1
      | _ -> 0)
  in
  let passed = List.map point (Test.exec_tests !tests) in
  Test.run_tests ~output:(Test.Html_output (open_out "result.html")) !tests;
  Test.run_tests !tests;
  prerr_endline @@ "### GRADES: " ^ String.concat " "
  @@ List.map2 (fun x y -> string_of_int (y*x)) passed !points