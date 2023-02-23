{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE PartialTypeSignatures     #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE UndecidableInstances      #-}

{-# OPTIONS_GHC -Wall #-}

{-# OPTIONS_GHC -Werror                        #-}
{-# OPTIONS_GHC -Wincomplete-record-updates    #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures   #-}
{-# OPTIONS_GHC -Wno-type-defaults             #-}
{-# OPTIONS_GHC -Wno-unused-top-binds          #-}

#if __GLASGOW_HASKELL__ >= 902
{-# OPTIONS_GHC -Wno-ambiguous-fields #-}
#endif
