foo :: Foo -> Maybe Bar
foo = \case
  Foo -> Just Bar
  _ -> Nothing
