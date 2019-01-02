import TranslatorInterpreter
import Translator

main = do
    trans <- interpret_translator "filetype tex"
    putStrLn $ trans_convert_filepath trans "test.hyp"
