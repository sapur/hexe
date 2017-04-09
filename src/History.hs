module History (
    History, emptyHistory,
    getStats,
    setVersion, undo, redo, trim
) where


data History t = History
    { histPast    :: [t]
    , histPresent :: Maybe t
    , histFuture  :: [t]
    , histEOT     :: Maybe t
    }
    deriving (Eq, Show, Read)


emptyHistory = History
    { histPast    = []
    , histPresent = Nothing
    , histFuture  = []
    , histEOT     = Nothing
    }

getStats hist =
    ( length (histPast hist)
    , length (histFuture hist)
    )

setVersion cur hist = hist
    { histPast    = cur : histPast hist
    , histPresent = Nothing
    , histFuture  = []
    , histEOT     = Nothing
    }

undo cur hist = case hist of
    History (x:xs) Nothing [] Nothing ->
        (x, History xs (Just x) [] (Just cur))
    History (x:xs) (Just a) ys b ->
        (x, History xs (Just x) (a:ys) b)
    History [] _ _ _ ->
        (cur, hist)

redo cur hist = case hist of
    History xs (Just a) [] (Just b) ->
        (b, History (a:xs) Nothing [] Nothing)
    History xs (Just a) (y:ys) b ->
        (y, History (a:xs) (Just y) ys b)
    History _ Nothing _ _ ->
        (cur, hist)

trim _ = emptyHistory
