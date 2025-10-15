module Exports exposing (elmJsonToProjectAndDependencySourceDirectories, locationDelta, packageSourceDirectoryPath)

import Elm.Constraint
import Elm.Package
import Elm.Project
import Elm.Version
import ElmSyntax
import Json.Decode
import TextGrid


elmJsonToProjectAndDependencySourceDirectories :
    { homeDirectory : Maybe String }
    -> String
    -> Result String (List String)
elmJsonToProjectAndDependencySourceDirectories environment elmJsonSource =
    case elmJsonSource |> Json.Decode.decodeString Elm.Project.decoder of
        Err elmJsonReadError ->
            Err (Json.Decode.errorToString elmJsonReadError)

        Ok elmJson ->
            Ok
                (case elmJson of
                    Elm.Project.Application application ->
                        case environment.homeDirectory of
                            Nothing ->
                                application.dirs

                            Just homeDirectoryPath ->
                                application.dirs
                                    ++ ((application.depsDirect
                                            ++ application.depsIndirect
                                        )
                                            |> List.map
                                                (\( dependencyName, dependencyVersion ) ->
                                                    packageSourceDirectoryPath
                                                        { homeDirectory = homeDirectoryPath
                                                        , packageName = dependencyName |> Elm.Package.toString
                                                        , packageVersion = dependencyVersion |> Elm.Version.toString
                                                        }
                                                )
                                       )

                    Elm.Project.Package package ->
                        "src"
                            :: (case environment.homeDirectory of
                                    Nothing ->
                                        []

                                    Just homeDirectoryPath ->
                                        package.deps
                                            |> List.map
                                                (\( dependencyName, dependencyVersion ) ->
                                                    packageSourceDirectoryPath
                                                        { homeDirectory = homeDirectoryPath
                                                        , packageName = dependencyName |> Elm.Package.toString
                                                        , packageVersion =
                                                            dependencyVersion
                                                                |> -- dear Evan, please expose the variant Elm.Constraint.Constraint
                                                                   Elm.Constraint.toString
                                                                |> -- relies on that fact that currently
                                                                   -- Elm.Constraint.toString currently puts spaces around the comparison operator
                                                                   String.split " "
                                                                |> List.head
                                                                |> Maybe.withDefault "1.0.0"
                                                        }
                                                )
                               )
                )


packageSourceDirectoryPath :
    { homeDirectory : String, packageName : String, packageVersion : String }
    -> String
packageSourceDirectoryPath info =
    info.homeDirectory
        ++ "/.elm/0.19.1/packages/"
        ++ info.packageName
        ++ "/"
        ++ info.packageVersion
        ++ "/src"


{-| The resulting column is the 0-based char offset after the resulting number of lines
-}
locationDelta :
    TextGrid.Location
    -> TextGrid.Location
    -> { line : Int, column : Int }
locationDelta earlier later =
    if earlier.line == later.line then
        { line = 0, column = later.column - earlier.column }

    else
        { line = later.line - earlier.line, column = later.column - 1 }
