
type 'a t = {
  queue: 'a Queue.t;
  condition: unit Lwt_condition.t;
}

let create (): 'a t = {
  queue = Queue.create ();
  condition = Lwt_condition.create ()
}

let add (q: 'a t) (x: 'a): unit =
  Queue.add x q.queue;
  Lwt_condition.signal q.condition ()

let rec take (q: 'a t): 'a Lwt.t =
  if Queue.is_empty q.queue then
    lwt () = Lwt_condition.wait q.condition in
    take q
  else
    Lwt.return (Queue.take q.queue)

