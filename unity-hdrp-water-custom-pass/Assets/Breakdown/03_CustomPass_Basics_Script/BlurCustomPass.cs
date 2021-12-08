using UnityEngine;
using UnityEngine.Experimental.Rendering;
using UnityEngine.Rendering;
using UnityEngine.Rendering.HighDefinition;
using Vector2 = UnityEngine.Vector2;

public class BlurCustomPass : CustomPass
{
    [Range(0, 64)] public float blurRadius = 1;
    [Range(2, 64)] public int sampleCount = 10;
    public bool downsampleBuffer = true;

    private RTHandle TempBuffer { get; set; }

    protected override void Setup(ScriptableRenderContext renderContext, CommandBuffer cmd)
    {
        base.Setup(renderContext, cmd);

        TempBuffer = RTHandles.Alloc(
            Vector2.one, 
            TextureXR.slices, dimension: TextureXR.dimension,
            name: "BlurBuffer"
        );
    }

    protected override void Execute(CustomPassContext ctx)
    {
        base.Execute(ctx);

        var resRadius = blurRadius * ctx.cameraColorBuffer.rtHandleProperties.rtHandleScale.x;
        
        CustomPassUtils.GaussianBlur(
            ctx, 
            ctx.cameraColorBuffer, 
            ctx.cameraColorBuffer, 
            TempBuffer, 
            sampleCount,
            resRadius, 
            downSample: downsampleBuffer);
    }

    protected override void Cleanup()
    {
        base.Cleanup();

        TempBuffer.Release();
        TempBuffer = null;
    }
}