public partial class LabeLConfig
{
    private LabeLConfig(bool IsLabelChooseByServer,string DefaultLabelCode,bool CheckSettingOnPrint)
    {
        this.IsLabelChooseByServer = IsLabelChooseByServer;
        this.DefaultLabelCode = DefaultLabelCode;
        this.CheckSettingOnPrint = CheckSettingOnPrint;
    }
}