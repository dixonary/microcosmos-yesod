module Main where
-- Web
import Prelude hiding (readFile, unlines, lines, all, fail)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types.Status
import Yesod.Core hiding ((.=), FileInfo, fileName, getCurrentTime, Header)
import Yesod.Static
-- System
import System.Directory
import System.Environment
import System.FilePath
import Data.Time.Format
import Data.Time.Format.Human
import Data.Time.Clock
-- Text
import Data.Text as Text (Text, strip, pack, unpack)
import Data.Text.IO (readFile)
import Data.Char (toLower)
import Text.Hamlet
import Text.Blaze.Html (preEscapedToHtml)
import Text.Hamlet.Runtime
import Text.Megaparsec hiding (many, some)
import Text.Megaparsec.Char
import Text.Pandoc hiding (FileInfo, getModificationTime, getCurrentTime)
import Text.Pandoc.Extensions
-- Data structures & Control flow
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Control.Applicative
import Control.Monad hiding (fail)
import Control.Monad.Fail
import Data.Void
import Data.Bifunctor (second)
import Data.Function ((&))

--------------------------------------------------------------------------------
-- Types and Yesod Preliminaries

data MC = MC { getStatic :: Static }

mkYesod "MC" [parseRoutes|
/               HomeR       GET
/static         StaticR     Static getStatic
/favicon.ico    FaviconR    GET
!/*[Text]       PostsR      GET
|]

instance Yesod MC where
    defaultLayout = layout

data Dir = Dir
    { dirPath :: FilePath
    , dirName :: Text
    , dirTitle :: Text
    }

data Post = Post
    { postName      :: Text
    , postExtension :: Text
    , postTitle     :: Text
    , postDate      :: UTCTime
    , postContent   :: Text
    }

type Parser a = Parsec Void Text a
type Header = (Text, Text)
type Headers = Map Text Text

root :: FilePath
root = "posts"

--------------------------------------------------------------------------------
-- Routes

instance MonadFail Handler where
    fail s = sendResponseStatus status500 $ pack s


layout :: Widget -> Handler Html
layout widget = do
    renderer <- getUrlRenderParams
    pc       <- widgetToPageContent widget
    html     <- runtimeHamlet
                "templates/layout.hamlet"
                [ "pageTitle" .= pageTitle pc
                , "pageHead"  .= pageHead  pc renderer
                , "pageBody"  .= pageBody  pc renderer
                ]
    sendResponse html


-- Given an extensionless filepath, find the post(s).
getPost :: FilePath -> Handler (Maybe Post)
getPost path = do
    if (null (takeBaseName path) || head (takeBaseName path) == '.')
        then return Nothing
        else do
            res <- liftIO $ doesDirectoryExist $ dropFileName $ root </> path
            if not res then return Nothing
            else do
                allPaths <- liftIO $ listDirectory $ dropFileName $ root </> path

                paths <- liftIO $ allPaths
                          & filter (\p -> takeFileName path == dropExtension p)
                          & filterM (liftM not . doesDirectoryExist . (root </>))
                let pathM = listToMaybe paths

                case pathM of
                    Nothing -> return Nothing
                    Just p -> do
                        getPostFull $ dropFileName path </> p


-- Given a full filepath, read the post(s).
getPostFull :: FilePath -> Handler (Maybe Post)
getPostFull path = do
    let
        filename = takeBaseName $ dropExtension path
        fileExt  = takeExtension path
        fullPath = root </> path

    res <- liftIO $ doesFileExist fullPath
    if not res then return Nothing
    else do
        contentE <- liftIO $ getPostData fullPath
        case contentE of
            Nothing -> return Nothing
            Just (headers, body) -> do
                mtime <- liftIO $ getModificationTime fullPath

                let
                    title = Map.findWithDefault (pack filename) "title" headers
                    parseDateM d =
                        Prelude.foldr1 (<|>)
                        $ (\x -> parseTimeM True defaultTimeLocale x (unpack d))
                        <$> (["%D %R", "%F %R", "%D", "%F"]::[String])

                    date = fromMaybe mtime $ join
                        $ parseDateM <$> Map.lookup "date" headers

                return $ Just Post
                    { postName      = pack filename
                    , postExtension = pack fileExt
                    , postTitle     = title
                    , postDate      = date
                    , postContent   = body
                    }


getDir :: FilePath -> IO (Maybe Dir)
getDir path = do
    exists <- doesDirectoryExist $ root </> path
    if not exists then return Nothing
    else do
        let basename    = pack $ takeBaseName path
            infoPath    = root </> path </> ".dirinfo"
        hasDirinfo <- doesFileExist infoPath
        title      <- if hasDirinfo
                        then readFile infoPath
                        else return basename
        return $ Just $ Dir path basename title


getPostsR :: [Text] -> Handler Html
getPostsR pathPieces = do
    let path = foldl (</>) "" $ unpack <$> pathPieces

    dirM <- liftIO $ getDir path
    postM <- getPost path
    () <- case postM of
        Nothing -> return ()
        Just post -> void $ sendPost post

    case dirM of
        Nothing -> return ()
        Just dir -> void $ sendDir dir

    notFound


getPostData :: FilePath -> IO (Maybe (Headers, Text))
getPostData path = do
    post <- readFile path
    return $ parseMaybe postP post

    where
        postP    = (,) <$> headersP <*> takeRest
        fenceP   = manyTill (char '-') (eol::Parser Text)
        headersP = Map.fromList
            <$> option [] (between fenceP fenceP (many headerP))
        headerP  = do
            l <- manyTill (satisfy (/= '-'))     $ char ':'
            r <- manyTill (satisfy (const True)) $ eol
            return (Text.strip $ pack l,Text.strip $ pack r)


sendDir :: Dir -> Handler Html
sendDir Dir{..} = do
    filenames <- liftIO $ listDirectory $ root </> dirPath

    dirs <- liftIO $ catMaybes <$> mapM (getDir . (dirPath </>)) filenames
    posts <- catMaybes <$> mapM (getPost . (dirPath </>) . dropExtension) filenames

    let renderedDirs = flip map dirs $ const $ let
            fullDir = "/" <> dirPath </> unpack dirName in
            [shamlet|
            <a href=#{fullDir}>
                <.dir>
                    <p.dir-name>#{dirTitle} |]
        renderedPosts = flip map posts $ \Post{..} -> let
            fullPost = "/" <> dirPath </> unpack postName
            in [shamlet|
            <a href=#{fullPost}>
                <.post>
                    <p.post-name>#{postTitle} |]

    html <- runtimeHamlet "templates/folder.hamlet"
        [ "dirname" .= dirTitle
        , "dirs"    .= toHamletData <$> renderedDirs
        , "posts"   .= toHamletData <$> renderedPosts
        ]

    defaultLayout $ toWidgetBody html


sendPost :: Post -> Handler Html
sendPost Post{..} = do
    let reader :: Either String (Reader PandocIO, Extensions)
        reader = fmap ( second (<> exts) ) $ getReader =<<
            case formatFromFilePath (unpack postName <.> unpack postExtension) of
                Nothing -> Left "Nothing"
                Just s  -> Right s

        exts = extensionsFromList
            [ Ext_abbreviations
            , Ext_auto_identifiers
            , Ext_backtick_code_blocks
            , Ext_citations
            , Ext_definition_lists
            , Ext_emoji
            , Ext_fancy_lists
            , Ext_fenced_divs
            , Ext_fenced_code_blocks
            , Ext_fenced_code_attributes
            , Ext_footnotes
            , Ext_grid_tables
            , Ext_header_attributes
            , Ext_latex_macros
            , Ext_raw_html
            , Ext_gfm_auto_identifiers
            , Ext_grid_tables
            , Ext_link_attributes
            , Ext_pipe_tables
            , Ext_markdown_attribute
            -- , Ext_markdown_in_html_blocks
            , Ext_native_divs
            , Ext_native_spans
            , Ext_old_dashes
            , Ext_shortcut_reference_links
            , Ext_simple_tables
            , Ext_strikeout
            , Ext_superscript
            , Ext_subscript
            , Ext_styles
            , Ext_tex_math_dollars
            ]

        defExtsR = let
            k = def
            in k{readerExtensions = readerExtensions k <> exts}

        defExtsW = let
            k = def
            in k{writerExtensions = writerExtensions k <> exts}


    case reader of
        Left _ -> sendResponse postContent
        Right (r,_) -> do
            case r of
                TextReader tr -> do
                    html <- liftIO $ runIO $ tr defExtsR postContent
                                >>= writeHtml5 defExtsW

                    case html of
                        Left _ -> sendResponseStatus status500 ("Error"::Text)
                        Right body -> do
                            hrt <- liftIO $ humanReadableTime postDate

                            renderedPost <- runtimeHamlet "templates/post.hamlet"
                                [ "title" .= postTitle
                                , "body"  .= body
                                , "date"  .= pack hrt
                                , "extension" .= postExtension
                                ]

                            defaultLayout $ do
                                setTitle $ toHtml postTitle
                                toWidgetBody $ renderedPost
                _ -> sendResponseStatus status500 ("Error"::Text)


getFaviconR :: Handler TypedContent
getFaviconR = sendFile "image/x-icon" "static/favicon.ico"

getHomeR :: Handler Html
getHomeR = getPostsR ["home"]

--------------------------------------------------------------------------------
-- Initialization

main :: IO ()
main = do
    args <- getArgs
    let port = case args of
            []    -> 80
            (p:_) -> read p

    stt <- static "static"
    app <- toWaiApp $ MC stt

    run port app


--------------------------------------------------------------------------------
-- Helper functions

infixr 3 .=
(.=) :: ToHamletData b => a -> b -> (a, HamletData)
a .= b = (a, toHamletData b)

runtimeHamlet :: FilePath -> Map Text HamletData -> Handler Html
runtimeHamlet path dataMap = do
    template <- readHamletTemplateFile defaultHamletSettings path
    renderHamletTemplate template dataMap

formatFromFilePath :: FilePath -> Maybe String
formatFromFilePath x =
  case takeExtension (map toLower x) of
    ".adoc"     -> Just "asciidoc"
    ".asciidoc" -> Just "asciidoc"
    ".context"  -> Just "context"
    ".ctx"      -> Just "context"
    ".db"       -> Just "docbook"
    ".doc"      -> Nothing  -- Send the file as-is
    ".docx"     -> Just "docx"
    ".dokuwiki" -> Just "dokuwiki"
    ".epub"     -> Just "epub"
    ".fb2"      -> Just "fb2"
    ".htm"      -> Just "html"
    ".html"     -> Just "html"
    ".icml"     -> Just "icml"
    ".json"     -> Just "json"
    ".latex"    -> Just "latex"
    ".lhs"      -> Just "markdown+lhs"
    ".ltx"      -> Just "latex"
    ".markdown" -> Just "markdown"
    ".md"       -> Just "markdown"
    ".ms"       -> Just "ms"
    ".muse"     -> Just "muse"
    ".native"   -> Just "native"
    ".odt"      -> Just "odt"
    ".opml"     -> Just "opml"
    ".org"      -> Just "org"
    ".pdf"      -> Nothing -- Send the file as-is
    ".pptx"     -> Just "pptx"
    ".roff"     -> Just "ms"
    ".rst"      -> Just "rst"
    ".rtf"      -> Just "rtf"
    ".s5"       -> Just "s5"
    ".t2t"      -> Just "t2t"
    ".tei"      -> Just "tei"
    ".tei.xml"  -> Just "tei"
    ".tex"      -> Just "latex"
    ".texi"     -> Just "texinfo"
    ".texinfo"  -> Just "texinfo"
    ".text"     -> Just "markdown"
    ".textile"  -> Just "textile"
    ".txt"      -> Just "markdown"
    ".wiki"     -> Just "mediawiki"
    ".xhtml"    -> Just "html"
    ['.',y]     | y `elem` (['1'..'9']::[Char]) -> Just "man"
    _           -> Nothing
