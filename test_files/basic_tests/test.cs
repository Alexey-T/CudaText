using System;
using System.Collections.Generic;

namespace Burray
{
    public class HotWaterGuidedSchedulingScreen : CommonScreen
    {
        //Test
        #region Public Interface

        public void SetFirstScreen()
        {
            currentScreenIndex = ScreensEnum.HVAC_GS1;
            backStack.Clear();
        }

        public List<DayOfWeek> SelectedDaysOfWeek
        {
            set
            {
                if (value == null)
                {
                    selectedDaysOfWeek.Clear();
                }
                else
                {
                    if (value.Count >
                        PublicMethods.GetEnumLength<DayOfWeek>())
                    {
                        throw new ArgumentException("Week too long");
                    }

                    foreach (DayOfWeek dow in value)
                    {
                        if (PublicMethods.IsValidEnumField<DayOfWeek>(dow)
                            == false)
                        {
                            throw new ArgumentException("Invalid day: " +
                                                        (int)dow);
                        }
                    }

                    selectedDaysOfWeek = value;
                }
            }
        }

        #endregion Public Interface

        #region Private Data-Structure

        private static enum ScreensEnum
        {
            HVAC_GS1 = 0,
            HVAC_GS2,
            HVAC_GS3,
            HVAC_GS4,
            HVAC_GS5,
            HVAC_GS6,
            HVAC_GS7,
            HVAC_GS8,
            HVAC_GS9,
        }

        /// <summary>
        /// Data-Structure class that stores all info from each screen.
        /// </summary>
        private class ScreenData
        {
            public bool HasButtons;
            public string TopLabel;
            public string ControlTitle;
            public DateTime ControlDateTime;
            public bool status;
        }

        public static void hello() {

        }

    }
}
