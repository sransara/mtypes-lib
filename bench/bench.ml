type message =
    | PopQ
    | PoppedQ of int64
    | PoppedAll of int
    | Store of int64
    | All of int64 list
    | Stored of int
    | Error of string

let max_pops = 1000
let m_replicas = 4
let m_sync_freq = 250
