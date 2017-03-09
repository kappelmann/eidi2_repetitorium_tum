open Event

(* Returns the value of the event that responds faster*)
val ch_faster : 'a event -> 'a event -> 'a

(* Returns the value of the event that responds later*)
val ch_slower : 'a event -> 'a event -> 'a

(* Returns the value of both events. 
 * The result of the first event should be stored in the first
 * value of the result tuple, the result of the second event
 * in the second value of the result tuple.*)
val ch_both : 'a event -> 'b event -> 'a * 'b

type w = Send of string
type r = Fetch | Receive of string

(* Starts a looping thread for a mailbox which saves its messages in a stack.
 * Returns two channels which accept Send or Fetch operations respectivly.
 * If a Send call is received, the sent message will be placed in front
 * of the stack. If a fetch call is received, the first message
 * of the stack will be sent back to the channel with a Receive message.
 * If there is no message in the queue, "empty" will be used as a message.*)
val start_box : unit -> (w channel * r channel)
