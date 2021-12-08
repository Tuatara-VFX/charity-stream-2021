using UnityEngine;
using UnityEngine.Rendering;
using UnityEngine.Rendering.HighDefinition;

public class DrawFullscreenCustomPass : CustomPass
{
    public Shader fullscreenShader;

    private Material FullscreenMaterial { get; set; }
    
    protected override void Setup(ScriptableRenderContext renderContext, CommandBuffer cmd)
    {
        base.Setup(renderContext, cmd);
        FullscreenMaterial = CoreUtils.CreateEngineMaterial(fullscreenShader);
    }

    protected override void Execute(CustomPassContext ctx)
    {
        base.Execute(ctx);

        CoreUtils.SetRenderTarget(ctx.cmd, ctx.cameraColorBuffer, ctx.cameraDepthBuffer);
        CoreUtils.DrawFullScreen(ctx.cmd, FullscreenMaterial, ctx.propertyBlock);
    }

    protected override void Cleanup()
    {
        base.Cleanup();
        CoreUtils.Destroy(FullscreenMaterial);
        FullscreenMaterial = null;
    }
}
