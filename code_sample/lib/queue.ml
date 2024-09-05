module Queue = Saturn_lockfree.Queue 

type 'a t = 
  {size : int Atomic.t; queue : 'a Queue.t}

let create () = 
  {size = Atomic.make 0; 
   queue = Queue.create ()}

let push t msg =
  Atomic.incr t.size;
  Queue.push t.queue msg

let pop t =
  match Queue.pop_opt t.queue with
  | Some elt ->
      Atomic.decr t.size; Some elt
  | None ->
      Atomic.set t.size 0; None

let size t = Atomic.get t.size

let is_empty t = Queue.is_empty t.queue
