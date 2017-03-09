open SparseVector

let _ = if empty<>[] then failwith "Your empty method is not working correctly"
let v = [1;2;3;0;1;0;5;(-10)]
let v = sb_vektor v
let expected = [(0,1);(1,2);(2,3);(4,1);(6,5);(7,(-10))]
let b = v=expected
let _ = if not b then failwith "Your sb_vektor method is not working correctly"
let t = set 3 0 v
let _ = if t<>v then failwith "Your set method is not working correctly"
let t = set 3 17 v
let expected = [(0,1);(1,2);(2,3);(3,17);(4,1);(6,5);(7,(-10))]
let _ = if t<>expected then failwith "Your set method is not working correctly"
let t = set 0 0 v
let expected = [(1,2);(2,3);(4,1);(6,5);(7,(-10))]
let _ = if t<>expected then failwith "Your set method is not working correctly"
let t = mul 0 v
let _ = if t<>[] then failwith "Your mul method is not working correctly"
let t = mul 2 v
let expected = [(0,2);(1,4);(2,6);(4,2);(6,10);(7,(-20))]
let _ = if t<>expected then failwith "Your mul method is not working correctly"
let w = sb_vektor [(-1);9;3;0;1;0]
let res = add_sb_vektor v w
let expected = [(1,11);(2,6);(4,2);(6,5);(7,(-10))]
let b = res=expected
let _ = if not b then failwith "Your add_sb_vektor method is not working correctly"
let res = mul_sb_vektor v w
let expected = 1*(-1)+2*9+3*3+1*1
let b = res=expected
let _ = if b then print_string "Everything is working. Nice job! ;) +5 points" else failwith "Your add_sb_vektor method is not working correctly"
