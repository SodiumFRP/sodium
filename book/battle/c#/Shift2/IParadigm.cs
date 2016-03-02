using System;

namespace Shift2
{
    public interface IParadigm : IDisposable
    {
        void HandleMouseDown(MouseEvtWithElement me);
        void HandleMouseMove(MouseEvt me);
        void HandleMouseUp(MouseEvt me);
        void HandleShift(bool isDown);
    }
}