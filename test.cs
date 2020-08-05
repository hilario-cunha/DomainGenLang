using Tlantic.Functional;
using System.Collections.Generic;
using System.Drawing;
namespace MRS.InStore.SDK
{
    public partial class Zone
    {
        public Zone(string zoneCode,string value)
        {
            this.ZoneCode = zoneCode;
            this.Value = value;
        }
        public string ZoneCode {get; private set;}
        public string Value {get; private set;}
    }
    public partial class WithdrawModalControl
    {
        public WithdrawModalControl(string code,bool visible)
        {
            this.Code = code;
            this.Visible = visible;
        }
        public string Code {get; private set;}
        public bool Visible {get; private set;}
    }
    public partial class TaskTypeDefinitionsWithCode
    {
        public TaskTypeDefinitionsWithCode(string taskTypeCode,TaskTypeDefinitions taskTypeDefinitions)
        {
            this.TaskTypeCode = taskTypeCode;
            this.TaskTypeDefinitions = taskTypeDefinitions;
        }
        public string TaskTypeCode {get; private set;}
        public TaskTypeDefinitions TaskTypeDefinitions {get; private set;}
    }
    public partial class TaskSearchInput
    {
        public TaskSearchInput(string code,string type,string[] value,Maybe<string> defaultValue)
        {
            this.Code = code;
            this.Type = type;
            this.Value = value;
            this.DefaultValue = defaultValue;
        }
        public string Code {get; private set;}
        public string Type {get; private set;}
        public string[] Value {get; private set;}
        public Maybe<string> DefaultValue {get; private set;}
    }
    public partial class TaskLocation
    {
        public TaskLocation(bool active,bool display,Maybe<List<string>> options)
        {
            this.Active = active;
            this.Display = display;
            this.Options = options;
        }
        public bool Active {get; private set;}
        public bool Display {get; private set;}
        public Maybe<List<string>> Options {get; private set;}
    }
    public partial class Range
    {
        public Range(int begin,int end)
        {
            this.Begin = begin;
            this.End = end;
        }
        public int Begin {get; private set;}
        public int End {get; private set;}
    }
    public partial class AlertColor
    {
        public AlertColor(Range range,Color color)
        {
            if (range == null)
                throw new System.ArgumentNullException("range","Field range with type Range can not be null");
            this.Range = range;
            if (color == null)
                throw new System.ArgumentNullException("color","Field color with type Color can not be null");
            this.Color = color;
        }
        public Range Range {get; private set;}
        public Color Color {get; private set;}
    }
    public partial class DestinationSearchInput
    {
        public DestinationSearchInput(string code,string type,string defaultValue)
        {
            this.Code = code;
            this.Type = type;
            this.DefaultValue = defaultValue;
        }
        public string Code {get; private set;}
        public string Type {get; private set;}
        public string DefaultValue {get; private set;}
    }
    public partial class DropdownOptions
    {
        public DropdownOptions(bool mandatory,List<string> options,string type)
        {
            this.Mandatory = mandatory;
            this.Options = options;
            this.Type = type;
        }
        public bool Mandatory {get; private set;}
        public List<string> Options {get; private set;}
        public string Type {get; private set;}
    }
    public partial class Reason
    {
        public Reason(string reasonCode,string value)
        {
            this.ReasonCode = reasonCode;
            this.Value = value;
        }
        public string ReasonCode {get; private set;}
        public string Value {get; private set;}
    }
    public partial class CreateTaskOptions
    {
        public CreateTaskOptions(CreateTaskOption[] options)
        {
            if (options == null)
                throw new System.ArgumentNullException("options","Field options with type CreateTaskOption[] can not be null");
            this.Options = options;
        }
        public CreateTaskOption[] Options {get; private set;}
    }
    public partial class CreateTaskAllOptions
    {
        public CreateTaskAllOptions(CreateTaskOption createTaskOption,string location,string destinationCode,string destinationValue)
        {
            this.CreateTaskOption = createTaskOption;
            this.Location = location;
            this.DestinationCode = destinationCode;
            this.DestinationValue = destinationValue;
        }
        public CreateTaskOption CreateTaskOption {get; private set;}
        public string Location {get; private set;}
        public string DestinationCode {get; private set;}
        public string DestinationValue {get; private set;}
    }
    public partial class AdhocTasks
    {
        public AdhocTasks(AdHocTasksDefaultNameV2 adHocTasksDefaultNameV2,AdhocTasksLevel adhocTasksLevel,CreateTaskOptions adhocTasksTypes)
        {
            this.AdHocTasksDefaultNameV2 = adHocTasksDefaultNameV2;
            this.AdhocTasksLevel = adhocTasksLevel;
            if (adhocTasksTypes == null)
                throw new System.ArgumentNullException("adhocTasksTypes","Field adhocTasksTypes with type CreateTaskOptions can not be null");
            this.AdhocTasksTypes = adhocTasksTypes;
        }
        public AdHocTasksDefaultNameV2 AdHocTasksDefaultNameV2 {get; private set;}
        public AdhocTasksLevel AdhocTasksLevel {get; private set;}
        public CreateTaskOptions AdhocTasksTypes {get; private set;}
    }
    public partial class SortResources
    {
        public SortResources(bool active,string defaultSort,List<string> options)
        {
            this.Active = active;
            this.DefaultSort = defaultSort;
            this.Options = options;
        }
        public bool Active {get; private set;}
        public string DefaultSort {get; private set;}
        public List<string> Options {get; private set;}
    }
    public partial class AdHocTasksDefaultNameV2
    {
        public AdHocTasksDefaultNameV2(bool active,string checklistExpression,string hsExpression,string withListExpression,string withoutListExpression)
        {
            this.Active = active;
            this.ChecklistExpression = checklistExpression;
            this.HsExpression = hsExpression;
            this.WithListExpression = withListExpression;
            this.WithoutListExpression = withoutListExpression;
        }
        public bool Active {get; private set;}
        public string ChecklistExpression {get; private set;}
        public string HsExpression {get; private set;}
        public string WithListExpression {get; private set;}
        public string WithoutListExpression {get; private set;}
    }
    public partial class AllowContainers
    {
        public AllowContainers(bool withoutLabel,ContainersLevel level)
        {
            this.WithoutLabel = withoutLabel;
            this.Level = level;
        }
        public bool WithoutLabel {get; private set;}
        public ContainersLevel Level {get; private set;}
    }
    public partial class GetItemAdhocInfo
    {
        public GetItemAdhocInfo(bool active,int timeout,bool destination)
        {
            this.Active = active;
            this.Timeout = timeout;
            this.Destination = destination;
        }
        public bool Active {get; private set;}
        public int Timeout {get; private set;}
        public bool Destination {get; private set;}
    }
    public partial class NotProcessedResourcesModal
    {
        public NotProcessedResourcesModal(bool notFoundButton,ResourceType resourceType)
        {
            this.NotFoundButton = notFoundButton;
            this.ResourceType = resourceType;
        }
        public bool NotFoundButton {get; private set;}
        public ResourceType ResourceType {get; private set;}
    }
    public partial class PreviousApproveAction
    {
        public PreviousApproveAction(PreviousApproveActionCode code,Maybe<PreviousApproveActionAttribute[]> previousApproveActionAttributeList)
        {
            this.Code = code;
            this.PreviousApproveActionAttributeList = previousApproveActionAttributeList;
        }
        public PreviousApproveActionCode Code {get; private set;}
        public Maybe<PreviousApproveActionAttribute[]> PreviousApproveActionAttributeList {get; private set;}
    }
    public partial class PreviousApproveActionAttribute
    {
        public PreviousApproveActionAttribute(string code,string type,string value,List<PreviousApproveActionAttributeOptions> options)
        {
            this.Code = code;
            this.Type = type;
            this.Value = value;
            this.Options = options;
        }
        public string Code {get; private set;}
        public string Type {get; private set;}
        public string Value {get; private set;}
        public List<PreviousApproveActionAttributeOptions> Options {get; private set;}
    }
    public partial class PreviousApproveActionAttributeOptions
    {
        public PreviousApproveActionAttributeOptions(string code,string value)
        {
            this.Code = code;
            this.Value = value;
        }
        public string Code {get; private set;}
        public string Value {get; private set;}
    }
    public partial class TaskTypeDefinitions
    {
        public TaskTypeDefinitions(AdhocTasks adhocTasks,Maybe<AlertColor[]> alertColors,AllowContainers allowContainers,bool allowOverQuantity,bool allowPickingStatus,bool askOnEqualPrice,bool checkConnectionOnStart,Difficulty difficulty,bool filterResources,GetItemAdhocInfo getItemAdhocInfo,bool handleQuantities,bool manageContainers,Maybe<IntPositive> maxEanLength,NotProcessedResourcesModal notProcessedResourcesModal,bool onAddResourceError,bool onGetItemAdhocInfo,bool onlyChangeQuantityByPicking,bool openModalOnPicking,bool processOnPicking,Maybe<List<Reason>> reasons,bool reasonMandatory,bool visible,Maybe<List<Zone>> zones,Maybe<List<DropdownOptions>> dropdownOptions,SummaryApprovalHandling summaryApprovalHandling,HideQuantities hideQuantities,bool replenishItemOnModal,LabelConfig labelConfig,bool showSOH,bool askLabelPrice,bool showPrint,bool hideResume,bool showContinueOnSelect,bool removeItemAdhoc,bool isToShowTaskEmptyWarning,bool sendFutureValidities,bool allowShelfQuantity,Maybe<TaskSearchInput[]> taskSearchInput,bool searchProductOnModal,bool askSecondEAN,bool allowExpectedQuantityDefault,string handleMode,bool showFutureDatesOnApproval,TaskLocation taskLocation,Maybe<WithdrawModalControl[]> withdrawModalControl,bool allowDepreciateOverSoh,bool allowCreateZonesAdhoc,bool showScheduledStartHour,bool allowDateOverLimit,PreviousApproveAction previousApproveAction,Maybe<DestinationSearchInput[]> destinationSearchInput,ListMode listMode,SortResources sortResources)
        {
            if (adhocTasks == null)
                throw new System.ArgumentNullException("adhocTasks","Field adhocTasks with type AdhocTasks can not be null");
            this.AdhocTasks = adhocTasks;
            this.AlertColors = alertColors;
            this.AllowContainers = allowContainers;
            this.AllowOverQuantity = allowOverQuantity;
            this.AllowPickingStatus = allowPickingStatus;
            this.AskOnEqualPrice = askOnEqualPrice;
            this.CheckConnectionOnStart = checkConnectionOnStart;
            this.Difficulty = difficulty;
            this.FilterResources = filterResources;
            this.GetItemAdhocInfo = getItemAdhocInfo;
            this.HandleQuantities = handleQuantities;
            this.ManageContainers = manageContainers;
            this.MaxEanLength = maxEanLength;
            if (notProcessedResourcesModal == null)
                throw new System.ArgumentNullException("notProcessedResourcesModal","Field notProcessedResourcesModal with type NotProcessedResourcesModal can not be null");
            this.NotProcessedResourcesModal = notProcessedResourcesModal;
            this.OnAddResourceError = onAddResourceError;
            this.OnGetItemAdhocInfo = onGetItemAdhocInfo;
            this.OnlyChangeQuantityByPicking = onlyChangeQuantityByPicking;
            this.OpenModalOnPicking = openModalOnPicking;
            this.ProcessOnPicking = processOnPicking;
            this.Reasons = reasons;
            this.ReasonMandatory = reasonMandatory;
            this.Visible = visible;
            this.Zones = zones;
            this.DropdownOptions = dropdownOptions;
            this.SummaryApprovalHandling = summaryApprovalHandling;
            this.HideQuantities = hideQuantities;
            this.ReplenishItemOnModal = replenishItemOnModal;
            this.LabelConfig = labelConfig;
            this.ShowSOH = showSOH;
            this.AskLabelPrice = askLabelPrice;
            this.ShowPrint = showPrint;
            this.HideResume = hideResume;
            this.ShowContinueOnSelect = showContinueOnSelect;
            this.RemoveItemAdhoc = removeItemAdhoc;
            this.IsToShowTaskEmptyWarning = isToShowTaskEmptyWarning;
            this.SendFutureValidities = sendFutureValidities;
            this.AllowShelfQuantity = allowShelfQuantity;
            this.TaskSearchInput = taskSearchInput;
            this.SearchProductOnModal = searchProductOnModal;
            this.AskSecondEAN = askSecondEAN;
            this.AllowExpectedQuantityDefault = allowExpectedQuantityDefault;
            this.HandleMode = handleMode;
            this.ShowFutureDatesOnApproval = showFutureDatesOnApproval;
            if (taskLocation == null)
                throw new System.ArgumentNullException("taskLocation","Field taskLocation with type TaskLocation can not be null");
            this.TaskLocation = taskLocation;
            this.WithdrawModalControl = withdrawModalControl;
            this.AllowDepreciateOverSoh = allowDepreciateOverSoh;
            this.AllowCreateZonesAdhoc = allowCreateZonesAdhoc;
            this.ShowScheduledStartHour = showScheduledStartHour;
            this.AllowDateOverLimit = allowDateOverLimit;
            this.PreviousApproveAction = previousApproveAction;
            this.DestinationSearchInput = destinationSearchInput;
            this.ListMode = listMode;
            if (sortResources == null)
                throw new System.ArgumentNullException("sortResources","Field sortResources with type SortResources can not be null");
            this.SortResources = sortResources;
        }
        public AdhocTasks AdhocTasks {get; private set;}
        public Maybe<AlertColor[]> AlertColors {get; private set;}
        public AllowContainers AllowContainers {get; private set;}
        public bool AllowOverQuantity {get; private set;}
        public bool AllowPickingStatus {get; private set;}
        public bool AskOnEqualPrice {get; private set;}
        public bool CheckConnectionOnStart {get; private set;}
        public Difficulty Difficulty {get; private set;}
        public bool FilterResources {get; private set;}
        public GetItemAdhocInfo GetItemAdhocInfo {get; private set;}
        public bool HandleQuantities {get; private set;}
        public bool ManageContainers {get; private set;}
        public Maybe<IntPositive> MaxEanLength {get; private set;}
        public NotProcessedResourcesModal NotProcessedResourcesModal {get; private set;}
        public bool OnAddResourceError {get; private set;}
        public bool OnGetItemAdhocInfo {get; private set;}
        public bool OnlyChangeQuantityByPicking {get; private set;}
        public bool OpenModalOnPicking {get; private set;}
        public bool ProcessOnPicking {get; private set;}
        public Maybe<List<Reason>> Reasons {get; private set;}
        public bool ReasonMandatory {get; private set;}
        public bool Visible {get; private set;}
        public Maybe<List<Zone>> Zones {get; private set;}
        public Maybe<List<DropdownOptions>> DropdownOptions {get; private set;}
        public SummaryApprovalHandling SummaryApprovalHandling {get; private set;}
        public HideQuantities HideQuantities {get; private set;}
        public bool ReplenishItemOnModal {get; private set;}
        public LabelConfig LabelConfig {get; private set;}
        public bool ShowSOH {get; private set;}
        public bool AskLabelPrice {get; private set;}
        public bool ShowPrint {get; private set;}
        public bool HideResume {get; private set;}
        public bool ShowContinueOnSelect {get; private set;}
        public bool RemoveItemAdhoc {get; private set;}
        public bool IsToShowTaskEmptyWarning {get; private set;}
        public bool SendFutureValidities {get; private set;}
        public bool AllowShelfQuantity {get; private set;}
        public Maybe<TaskSearchInput[]> TaskSearchInput {get; private set;}
        public bool SearchProductOnModal {get; private set;}
        public bool AskSecondEAN {get; private set;}
        public bool AllowExpectedQuantityDefault {get; private set;}
        public string HandleMode {get; private set;}
        public bool ShowFutureDatesOnApproval {get; private set;}
        public TaskLocation TaskLocation {get; private set;}
        public Maybe<WithdrawModalControl[]> WithdrawModalControl {get; private set;}
        public bool AllowDepreciateOverSoh {get; private set;}
        public bool AllowCreateZonesAdhoc {get; private set;}
        public bool ShowScheduledStartHour {get; private set;}
        public bool AllowDateOverLimit {get; private set;}
        public PreviousApproveAction PreviousApproveAction {get; private set;}
        public Maybe<DestinationSearchInput[]> DestinationSearchInput {get; private set;}
        public ListMode ListMode {get; private set;}
        public SortResources SortResources {get; private set;}
    }
    public partial class LabelConfig
    {
        public LabelConfig(bool isLabelChooseByServer,string defaultLabelCode,bool checkSettingOnPrint)
        {
            this.IsLabelChooseByServer = isLabelChooseByServer;
            this.DefaultLabelCode = defaultLabelCode;
            this.CheckSettingOnPrint = checkSettingOnPrint;
        }
        public bool IsLabelChooseByServer {get; private set;}
        public string DefaultLabelCode {get; private set;}
        public bool CheckSettingOnPrint {get; private set;}
    }
    public partial class ChangePasswordRequest
    {
        public ChangePasswordRequest(StringNotEmpty oldPassword,StringNotEmpty newPassword,StringNotEmpty newPasswordConfirmation)
        {
            this.OldPassword = oldPassword;
            this.NewPassword = newPassword;
            this.NewPasswordConfirmation = newPasswordConfirmation;
        }
        public StringNotEmpty OldPassword {get; private set;}
        public StringNotEmpty NewPassword {get; private set;}
        public StringNotEmpty NewPasswordConfirmation {get; private set;}
    }
}