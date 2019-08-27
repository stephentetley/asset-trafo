SELECT 
        chreq.change_request_id        AS [change_request_id],
        chreq.change_request_time      AS [request_time],
        chreq.change_request_type      AS [request_type],
        chreq.change_request_status    AS [request_status],
        chreq.comments  AS [comments]
FROM    change_request AS chreq
WHERE
        chreq.change_request_id = 148640
;