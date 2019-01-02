join_row tgtcode =
    let helper :: [TargetCode] -> TargetCode
        helper ls = case ls of
            [] -> ""
            (x:xs) -> "<td>" ++ x ++ "</td>" ++ helper xs
    in "<tr class=\"row\">" ++ helper tgtcode ++ "</tr>"

join_hrow tgtcode =
    let helper :: [TargetCode] -> TargetCode
        helper ls = case ls of
            [] -> ""
            (x:xs) -> "<th>" ++ x ++ "</th>" ++ helper xs
    in "<tr class=\"hrow\">" ++ helper tgtcode ++ "</tr>"

