open Code_sample


let work1 barrier q = 
  Atomic.decr barrier;
  while (Atomic.get barrier <> 0 ) do Domain.cpu_relax () done;
    Queue.push q 1
  

  let work2 barrier q = 
    Atomic.decr barrier;
    while (Atomic.get barrier <> 0 ) do Domain.cpu_relax () done;
    (Queue.pop q) |> ignore

let test () =
  let queue = Queue.create () in
  let barrier = Atomic.make 2 in
  let domain1 = Domain.spawn (fun () -> work1 barrier queue) in 
  let domain2 = Domain.spawn (fun () -> work2 barrier queue) in 
  Domain.join domain1;
  Domain.join domain2;
  Queue.pop queue |> ignore;
  Queue.size queue

  let work1 barrier q = 
    Atomic.decr barrier;
    while (Atomic.get barrier <> 0 ) do Domain.cpu_relax () done;
    for i = 0 to 99 do Queue.push q i done
  
let work2 barrier q = 
    Atomic.decr barrier;
    while (Atomic.get barrier <> 0 ) do Domain.cpu_relax () done;
    for _ = 0 to 99 do Queue.pop q |> ignore done
    
  let another_test () =
    let queue = Queue.create () in
    let barrier = Atomic.make 2 in
    let domain1 = Domain.spawn (fun () -> work1 barrier queue) in 
    let domain2 = Domain.spawn (fun () -> work2 barrier queue) in 
    Domain.join domain1;
    Domain.join domain2;
    Queue.size queue

let () = 
  let count = ref 0 in
  let retry = 10000 in
  for _ = 1 to retry do
   let result = test () in
   if result < 0 then incr count
  done;
  Format.printf "T1 Fail rate %.2f %% \n" (float_of_int !count /. (float_of_int retry) *. 100.)


let () = 
  let count = ref 0 in
  let retry = 10000 in
  for _ = 1 to retry do
   let result = another_test () in
   if result < 0 then incr count
  done;
  Format.printf "T2 Fail rate %.2f %% \n" (float_of_int !count /. (float_of_int retry) *. 100.)
