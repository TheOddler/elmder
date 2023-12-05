module StringExtra exposing (..)

import Generated.Backend as Backend


fromGenderIdentity : Backend.GenderIdentity -> String
fromGenderIdentity gi =
    case gi of
        Backend.Abinary ->
            "Abinary"

        Backend.Agender ->
            "Agender"

        Backend.Ambigender ->
            "Ambigender"

        Backend.Androgyne ->
            "Androgyne"

        Backend.Androgynous ->
            "Androgynous"

        Backend.Aporagender ->
            "Aporagender"

        Backend.Autigender ->
            "Autigender"

        Backend.Bakla ->
            "Bakla"

        Backend.Bigender ->
            "Bigender"

        Backend.Binary ->
            "Binary"

        Backend.Bissu ->
            "Bissu"

        Backend.Butch ->
            "Butch"

        Backend.Calabai ->
            "Calabai"

        Backend.Calalai ->
            "Calalai"

        Backend.CisFemale ->
            "Cis female"

        Backend.CisMale ->
            "Cis male"

        Backend.CisMan ->
            "Cis man"

        Backend.CisWoman ->
            "Cis woman"

        Backend.DemiBoy ->
            "Demiboy"

        Backend.Demiflux ->
            "Demiflux"

        Backend.Demigender ->
            "Demigender"

        Backend.DemiGirl ->
            "Demi girl"

        Backend.DemiGuy ->
            "Demi guy"

        Backend.DemiMan ->
            "Demi man"

        Backend.DemiWoman ->
            "Demi woman"

        Backend.DualGender ->
            "Dual gender"

        Backend.FaAfafine ->
            "Fa'afafine"

        Backend.Female ->
            "Female"

        Backend.FemaleToMale ->
            "Female to male"

        Backend.Femme ->
            "Femme"

        Backend.FTM ->
            "FTM"

        Backend.GenderBender ->
            "Gender bender"

        Backend.GenderDiverse ->
            "Gender diverse"

        Backend.GenderGifted ->
            "Gender gifted"

        Backend.Genderfae ->
            "Genderfae"

        Backend.Genderfluid ->
            "Genderfluid"

        Backend.Genderflux ->
            "Genderflux"

        Backend.Genderfuck ->
            "Genderfuck"

        Backend.Genderless ->
            "Genderless"

        Backend.GenderNonconforming ->
            "Gender non-conforming"

        Backend.Genderqueer ->
            "Genderqueer"

        Backend.GenderQuestioning ->
            "Gender questioning"

        Backend.GenderVariant ->
            "Gender variant"

        Backend.Graygender ->
            "Graygender"

        Backend.Hijra ->
            "Hijra"

        Backend.Intergender ->
            "Intergender"

        Backend.Intersex ->
            "Intersex"

        Backend.Kathoey ->
            "Kathoey"

        Backend.Mahu ->
            "Māhū"

        Backend.Male ->
            "Male"

        Backend.MaleToFemale ->
            "Male to female"

        Backend.Man ->
            "Man"

        Backend.ManOfTransExperience ->
            "Man of trans experience"

        Backend.Maverique ->
            "Maverique"

        Backend.MetaGender ->
            "Meta gender"

        Backend.MTF ->
            "MTF"

        Backend.Multigender ->
            "Multigender"

        Backend.Muxe ->
            "Muxe"

        Backend.Neither ->
            "Neither"

        Backend.Neurogender ->
            "Neurogender"

        Backend.Neutrois ->
            "Neutrois"

        Backend.NonBinary ->
            "Non-Binary"

        Backend.NonBinaryTransgender ->
            "Non-Binary Transgender"

        Backend.Omnigender ->
            "Omnigender"

        Backend.Other ->
            "Other"

        Backend.Pangender ->
            "Pangender"

        Backend.PersonOfTransgenderedExperience ->
            "Person of transgendered experience"

        Backend.Polygender ->
            "Polygender"

        Backend.Sekhet ->
            "Sekhet"

        Backend.ThirdGender ->
            "Third gender"

        Backend.TransFemale ->
            "Trans female"

        Backend.TransMale ->
            "Trans male"

        Backend.TransMan ->
            "Trans man"

        Backend.TransPerson ->
            "Trans person"

        Backend.TransWoman ->
            "Trans woman"

        Backend.TransgenderFemale ->
            "Transgender female"

        Backend.TransgenderMale ->
            "Transgender male"

        Backend.TransgenderMan ->
            "Transgender man"

        Backend.TransgenderPerson ->
            "Transgender person"

        Backend.TransgenderWoman ->
            "Transgender woman"

        Backend.Transfeminine ->
            "Transfeminine"

        Backend.Transmasculine ->
            "Transmasculine"

        Backend.TranssexualFemale ->
            "Transsexual female"

        Backend.TranssexualMale ->
            "Transsexual male"

        Backend.TranssexualMan ->
            "Transsexual man"

        Backend.TranssexualPerson ->
            "Transsexual person"

        Backend.TranssexualWoman ->
            "Transsexual woman"

        Backend.Travesti ->
            "Travesti"

        Backend.Trigender ->
            "Trigender"

        Backend.Tumtum ->
            "Tumtum"

        Backend.TwoSpirit ->
            "Two-Spirit"

        Backend.Vakasalewalewa ->
            "Vakasalewalewa"

        Backend.Waria ->
            "Waria"

        Backend.Winkte ->
            "Winkte"

        Backend.Woman ->
            "Woman"

        Backend.WomanOfTransExperience ->
            "Woman of Trans Experience"

        Backend.XGender ->
            "X-gender"

        Backend.XJenda ->
            "X-jenda"

        Backend.Xenogender ->
            "Xenogender"
