let src = Logs.Src.create "async_http"

module L = (val Logs.src_log src : Logs.LOG)
include L
