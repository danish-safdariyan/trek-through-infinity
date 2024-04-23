type t =
  | Daily
  | Weekly of int (* Weekly repetition with an interval *)
  | Monthly of int (* Monthly repetition with a day of month *)
  | Yearly of int (* Yearly repetition with a month and day of month *)
