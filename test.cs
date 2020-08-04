namespace MRS.InStore.SDK
{
    public partial class Zone
    {
        public Zone(string ZoneCode,string Value)
        {
            this.ZoneCode = ZoneCode;
            this.Value = Value;
        }
        public string ZoneCode {get; private set;}
        public string Value {get; private set;}
    }
    public partial class WithdrawModalControl
    {
        public WithdrawModalControl(string Code,bool Visible)
        {
            this.Code = Code;
            this.Visible = Visible;
        }
        public string Code {get; private set;}
        public bool Visible {get; private set;}
    }
}