using UnityEngine;
using UnityEngine.Experimental.Rendering;
using UnityEngine.Rendering;
using UnityEngine.Rendering.HighDefinition;

class WaterCustomPass : CustomPass
{
    [Range(0, 64)] public float blurRadius = 4;
    [Range(0, 64)] public int sampleCount = 4;
    public bool downsampleBuffer;
    public Material transparentFullscreenShader = null;
    public Mesh quad = null;

    RTHandle blurBuffer;

    protected override void Setup(ScriptableRenderContext renderContext, CommandBuffer cmd)
    {
        blurBuffer = RTHandles.Alloc(
            Vector2.one, TextureXR.slices, dimension: TextureXR.dimension,
            colorFormat: GraphicsFormat.R16G16B16A16_SNorm,
            useDynamicScale: true, name: "BlurBuffer"
        );  
    }

    protected override void Execute(CustomPassContext ctx)
    {
        // Blur the custom buffer:
        var resRadius = blurRadius * ctx.cameraColorBuffer.rtHandleProperties.rtHandleScale.x;
        CustomPassUtils.GaussianBlur(ctx, ctx.customColorBuffer.Value, ctx.customColorBuffer.Value, blurBuffer, sampleCount,
            resRadius, downSample:downsampleBuffer);
        
        // Choose water buffer location.
        var distanceToCamera = Mathf.Lerp(ctx.hdCamera.camera.nearClipPlane, ctx.hdCamera.camera.farClipPlane, 0.0001f);
        var trs = Matrix4x4.TRS(
            ctx.hdCamera.camera.transform.position + ctx.hdCamera.camera.transform.forward * distanceToCamera,
            ctx.hdCamera.camera.transform.rotation,
            Vector3.one * 100f);

        // Draw water in front of camera.
        var pass = transparentFullscreenShader.FindPass("Forward");
        if (pass == -1)
            pass = transparentFullscreenShader.FindPass("ForwardOnly");
        CoreUtils.SetRenderTarget(ctx.cmd, ctx.cameraColorBuffer, ctx.cameraDepthBuffer);
        ctx.cmd.DrawMesh(quad, trs, transparentFullscreenShader, 0, pass);
    }

    protected override void Cleanup()
    {
        blurBuffer.Release();
    }
}