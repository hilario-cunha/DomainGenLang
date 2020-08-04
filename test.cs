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
        public TaskSearchInput(string code,string type,string[] value,int defaultValue)
        {
            this.Code = code;
            this.Type = type;
            this.Value = value;
            this.DefaultValue = defaultValue;
        }
        public string Code {get; private set;}
        public string Type {get; private set;}
        public string[] Value {get; private set;}
        public int DefaultValue {get; private set;}
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
}