module Hlbc where

import Text.HTML.TagSoup
import Network.Curl.Download

type Query = String
type Titre = String

data Result = Result Titre

data Region = Alsace 
    | Aquitaine 
    | Auvergne 
    | Basse_normandie 
    | Bourgogne 
    | Bretagne 
    | Centre 
    | Champagne_ardenne 
    | Corse 
    | Franche_comte 
    | Haute_normandie 
    | Ile_de_france 
    | Languedoc_roussillon
    | Limousin 
    | Lorraine 
    | Midi_pyrenees
    | Nord_pas_de_calais 
    | Pays_de_la_loire 
    | Picardie
    | Poitou_charentes
    | Provence_alpes_cote_azur
    | Rhone_alpes 
    | Guadeloupe
    | Martinique
    | Guyane 
    | Reunion 
    deriving (Show, Eq, Ord)

data Categorie = Vehicules 
    | Voitures
    | Moto
    | Caravaning 
    | Utilitaires
    | Equipement_auto
    | Equipement_moto
    | Equipement_caravaning 
    | Nautisme 
    | Equipement_nautisme 
    | Immobilier 
    | Vente_immobilieres
    | Location
    | Colocation 
    | Location_vacances 
    | Bureaux_commerces
    | Multimedia 
    | Consoles_jeux_video
    | Informatique 
    | Image_son 
    | Telephonie 
    | Maison 
    | Ameublement 
    | Electromenager 
    | Art_table 
    | Decoration 
    | Linge_maison 
    | Bricolage 
    | Jardinage 
    | Vetements 
    | Chaussures 
    | Accessoires_bagagerie 
    | Montres_bijoux 
    | Equipement_bebe
    | Vetements_bebe
    | Loisirs 
    | Dvd_films 
    | Cd_musique 
    | Livres 
    | Animaux 
    | Velos 
    | Sports_hobbies 
    | Instruments_musique 
    | Collection 
    | Jeux_jouets 
    | Vins_gastronomie 
    | Emploi_services 
    | Materiel_professionnel 
    | Emploi 
    | Services 
    | Billetterie
    | Evenements 
    | Cours_particuliers
    | Autres 
    deriving (Show, Eq, Ord)

data LbcQuery = LbcQuery 
                    Categorie 
                    Region 
                    Query deriving (Show, Eq, Ord)

-- 
-- fonctions pour traiter les url
--

reg2req a = case a of
    Aquitaine -> req ++ "aquit"
    Ile_de_france -> req ++ "idf"
    where req = "reg="

cat2req a = case a of
    Chaussures -> req ++ "chaussure"
    Voitures -> req ++ "voiture"
    where req = "cat="

query2req :: LbcQuery -> String
query2req (LbcQuery c r q) = do 
    url ++ cat2req c ++ sep ++ reg2req r ++ sep ++ query
    where 
        url = "http://lbc.fr/?"
        sep = "&"
        query = "query=" ++ q

annonces :: LbcQuery -> [Result]
annonces _ = []
--
-- src <- openURI "http://www.leboncoin.fr/voitures/offres/ile_de_france/?f=a&th=1&q=scenic"
-- sections (~== "<div class=\"lbc\">") (parseTags $ rights [src]!!0)
