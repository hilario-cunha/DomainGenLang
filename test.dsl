u [Tlantic.Functional , System.Collections.Generic, System.Drawing]
n MRS.InStore.SDK

c Zone
p ZoneCode string
p Value string

c WithdrawModalControl
p Code string
p Visible bool

c TaskTypeDefinitionsWithCode
p TaskTypeCode string
p TaskTypeDefinitions TaskTypeDefinitions

c TaskSearchInput
p Code string
p Type string
p Value string[]
p DefaultValue int

c TaskLocation
p Active bool
p Display bool
p Options Maybe<List<string>> 

c Range
p Begin int
p End int

c AlertColor
p Range Range NotNull
p Color Color NotNull
