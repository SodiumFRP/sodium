using System;

namespace Battle
{
    public interface IParadigm : IDisposable
    {
        void HandleMouseDown(MouseEvtWithElement me);
        void HandleMouseMove(MouseEvt me);
        void HandleMouseUp(MouseEvt me);
    }
}