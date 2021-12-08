using UnityEngine;
using UnityEngine.Rendering;
using UnityEngine.Rendering.HighDefinition;

public class SimplestCustomPass : CustomPass
{
    [Header("Our properties")]
    public Color clearColor;
    public ClearFlag clearFlag;
    
    // Called on first activation for each camera
    protected override void Setup(ScriptableRenderContext renderContext, CommandBuffer cmd)
    {
        base.Setup(renderContext, cmd);
        Debug.Log("Setup");
    }

    // Called every frame for each camera
    protected override void Execute(CustomPassContext ctx)
    {
        base.Execute(ctx);
        Debug.Log($"Execute for camera {ctx.hdCamera.camera.GetInstanceID()}");
        
        CoreUtils.SetRenderTarget(ctx.cmd, ctx.cameraColorBuffer, ctx.cameraDepthBuffer, clearFlag, clearColor);
    }
    
    // Called on desactivation for each camera
    protected override void Cleanup()
    {
        Debug.Log("Cleanup");
        base.Cleanup();
    }
}