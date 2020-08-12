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
p DefaultValue Maybe<string>

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

c DestinationSearchInput
p Code string
p Type string
p DefaultValue string

c DropdownOptions
p Mandatory bool
p Options List<string>
p Type string

c Reason
p ReasonCode string
p Value string

c CreateTaskOptions
p Options CreateTaskOption[] NotNull

c CreateTaskAllOptions
p CreateTaskOption CreateTaskOption
p Location string
p DestinationCode string
p DestinationValue string

c AdhocTasks
p AdHocTasksDefaultNameV2 AdHocTasksDefaultNameV2
p AdhocTasksLevel AdhocTasksLevel 
p AdhocTasksTypes CreateTaskOptions NotNull

c SortResources
p Active bool
p DefaultSort string
p Options List<string>

c AdHocTasksDefaultNameV2
p Active bool
p ChecklistExpression string
p HsExpression string
p WithListExpression string
p WithoutListExpression string

c AllowContainers
p WithoutLabel bool
p Level ContainersLevel

c GetItemAdhocInfo
p Active bool
p Timeout int
p Destination bool

c NotProcessedResourcesModal
p NotFoundButton bool
p ResourceType ResourceType

c PreviousApproveAction
p Code PreviousApproveActionCode
p PreviousApproveActionAttributeList Maybe<PreviousApproveActionAttribute[]>

c PreviousApproveActionAttribute
p Code string
p Type string
p Value string
p Options List<PreviousApproveActionAttributeOptions>

c PreviousApproveActionAttributeOptions
p Code string
p Value string

c TaskTypeDefinitions
p AdhocTasks AdhocTasks NotNull
p AlertColors Maybe<AlertColor[]>
p AllowContainers AllowContainers
p AllowOverQuantity bool
p AllowPickingStatus bool
p AskOnEqualPrice bool
p CheckConnectionOnStart bool
p Difficulty Difficulty
p FilterResources bool
p GetItemAdhocInfo GetItemAdhocInfo
p HandleQuantities bool
p ManageContainers bool
p MaxEanLength Maybe<IntPositive>
p NotProcessedResourcesModal NotProcessedResourcesModal NotNull
p OnAddResourceError bool
p OnGetItemAdhocInfo bool
p OnlyChangeQuantityByPicking bool
p OpenModalOnPicking bool
p ProcessOnPicking bool
p Reasons Maybe<List<Reason>>
p ReasonMandatory bool
p Visible bool
p Zones Maybe<List<Zone>>
p DropdownOptions Maybe<List<DropdownOptions>>
p SummaryApprovalHandling SummaryApprovalHandling
p HideQuantities HideQuantities
p ReplenishItemOnModal bool
p LabelConfig LabelConfig
p ShowSOH bool
p AskLabelPrice bool
p ShowPrint bool
p HideResume bool
p ShowContinueOnSelect bool
p RemoveItemAdhoc bool
p IsToShowTaskEmptyWarning bool
p SendFutureValidities bool
p AllowShelfQuantity bool
p TaskSearchInput Maybe<TaskSearchInput[]>
p SearchProductOnModal bool
p AskSecondEAN bool
p AllowExpectedQuantityDefault bool
p HandleMode string
p ShowFutureDatesOnApproval bool
p TaskLocation TaskLocation NotNull
p WithdrawModalControl Maybe<WithdrawModalControl[]>
p AllowDepreciateOverSoh bool
p AllowCreateZonesAdhoc bool
p ShowScheduledStartHour bool
p AllowDateOverLimit bool
p PreviousApproveAction PreviousApproveAction
p DestinationSearchInput Maybe<DestinationSearchInput[]>
p ListMode ListMode
p SortResources SortResources NotNull

c LabelConfig
p IsLabelChooseByServer bool
p DefaultLabelCode string
p CheckSettingOnPrint bool

c ChangePasswordRequest
p OldPassword StringNotEmpty Required
p NewPassword StringNotEmpty Required,NotEquals OldPassword
p NewPasswordConfirmation StringNotEmpty Required,Equals NewPassword
