

SELECT  scheme.scheme_code  AS [SchemeCode], 
        COUNT(change_req.change_request_id) AS [ChangeCount]
FROM work_scheme AS scheme 
JOIN asset_change AS asset_ch 
    ON asset_ch.scheme_id = scheme.scheme_id
JOIN change_request AS change_req
    ON asset_ch.change_request_id = change_req.change_request_id
GROUP BY scheme.scheme_code
ORDER BY COUNT(change_req.change_request_id) DESC
;