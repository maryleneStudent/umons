{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Home where

import           Miso
import           Miso.String (ms)
import           Miso.String
import           Action
import           Html
import           Model
import           Routing
import qualified Data.Map                      as M
import           Data.Maybe

firstName :: Person -> MisoString
firstName (Person firstname _) = ms firstname

lastName :: Person -> MisoString
lastName (Person _ lastname) = ms lastname

viewLoginOrRegister :: Model -> View Action
viewLoginOrRegister Model {..} = view
  where
  view = div_ [ class_ "jumbotron bg-light text-left"
         ] [ h2_ [ class_ "text-center" ] [ text $ pack "Identification ou enregistrement" ]
            , p_ [ class_ "help-block" ] [ text $ pack "Si vous avez déjà un compte, veuillez vous identifier. Sinon, créez un nouveau compte." ]
            , if registerErrorMsg /= "" then  div_ []
                    [ div_ [ class_ "alert alert-danger" ] [ text registerErrorMsg ], br_ []
                    ]
              else text $ pack ""
            , if loginErrorMsg /= "" then div_ []
                    [ div_ [ class_ "alert alert-danger" ] [ text loginErrorMsg ], br_ []
                    ]
              else text $ pack ""
            , showBaseForm mUsername mPassword
            , if register then 
                    div_[] [ div_ []
                                [ text $ pack "Prénom"
                                , br_ []
                                , input_ [value_ mfirstname, onInput SetFirstName]
                                ]
                            , br_ []
                            , div_ []
                                [ text $ pack "Nom"
                                , br_ []
                                , input_ [value_ mlastname, onInput SetLastName]
                                ]
                            , br_ []
                            , div_ []
                                [ button_ [ class_ "btn btn-link", type_ "button", onClick ClickRegisterUser ] [ text $ pack "S'enregistrer" ]
                                ]
                     ]
           else
             div_ [][
             button_ attrs [ text $ pack "S'identifier" ]
           , button_ attrs2 [ text $ pack "S'enregistrer"]
         ]
      ]
      where
        attrs = [ onClick ClickLogIn
                , class_ $ pack "btn btn-primary"
                ]
        attrs2 = [ onClick DisplayRegistrationView
                 , class_ $ pack "btn btn-link"]

showBaseForm :: MisoString -> MisoString -> View Action
showBaseForm username password =
    div_ []
        [ div_ []
           [ text $ pack "Nom d'utilisateur"
            , br_ []
            , input_ [ placeholder_ "" , value_ username , onInput SetUsername ]
            ]
        , br_ []
        , div_ []
            [ text $ pack "Mot de passe"
            , br_ []
            , input_ [ type_ "password", placeholder_ "" , value_ password , onInput SetPassword ]
            ]
        , br_ []
        ]

