namespace PetrolPump
{
    public interface IPump
    {
        Outputs Create(Inputs inputs);
    }
}