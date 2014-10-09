import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.PackageDescription
import Distribution.Text (display)

addCppOption new lbi = 
   let pd = localPkgDescr lbi
   in case library pd of
       Nothing  -> lbi
       Just lib ->
           let libbi = libBuildInfo lib
               cpp   = new : cppOptions libbi
           in lbi { localPkgDescr = pd { library = Just lib { libBuildInfo = libbi { cppOptions = cpp } } } }

version (CompilerId _ v) = v

updateLBI lbi =
   let v = display . version . compilerId $ compiler lbi
       f = "-DCOMPILER_VERSION=\"" ++ v ++ "\""
   in addCppOption f lbi 

main = defaultMainWithHooks
    simpleUserHooks { confHook = \i f -> updateLBI `fmap` confHook simpleUserHooks i f }
