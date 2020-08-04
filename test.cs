public partial class LabeLConfig
{
    public static Choice<LabeLConfig, LabeLConfigError> Create(bool IsLabelChooseByServer,string DefaultLabelCode,bool CheckSettingOnPrint)
    {
        if (DefaultLabelCode == null)
            throw new System.ArgumentNullException("DefaultLabelCode");
        if (DefaultLabelCode.Length > 10)
            return Choice<LabeLConfig, LabeLConfigError>.Choice2Of2(LabeLConfigError.MaxLengthDefaultLabelCodeError);
        return Choice<LabeLConfig, LabeLConfigError>.Choice1Of2(new LabeLConfig(IsLabelChooseByServer,DefaultLabelCode,CheckSettingOnPrint));
    }
    private LabeLConfig(bool IsLabelChooseByServer,string DefaultLabelCode,bool CheckSettingOnPrint)
    {
        this.IsLabelChooseByServer = IsLabelChooseByServer;
        this.DefaultLabelCode = DefaultLabelCode;
        this.CheckSettingOnPrint = CheckSettingOnPrint;
    }
    public bool IsLabelChooseByServer {get; private set;}
    public string DefaultLabelCode {get; private set;}
    public bool CheckSettingOnPrint {get; private set;}
}