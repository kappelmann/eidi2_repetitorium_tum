open Future
let f x = x*100-1
let c = create f 1
let r = get c
let _ = if r=99 then print_string "It's working! Now head on to the ThreadedTree.\n" else failwith "Your future does not seem to work."
