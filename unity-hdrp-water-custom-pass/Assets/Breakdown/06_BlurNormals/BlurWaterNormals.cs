using UnityEngine;
using UnityEngine.Experimental.Rendering;
using UnityEngine.Rendering;
using UnityEngine.Rendering.HighDefinition;

public class BlurWaterNormals : CustomPass
{
    public Material drawMaterial;
    public Mesh drawMesh;
    
    [Range(0, 64)] public float blurRadius = 1;
    [Range(2, 64)] public int sampleCount = 10;
    public bool downsampleBuffer = true;

    private RTHandle TempBuffer { get; set; }
    
    protected override void Setup(ScriptableRenderContext renderContext, CommandBuffer cmd)
    {
        base.Setup(renderContext, cmd);
        
        TempBuffer = RTHandles.Alloc(
            Vector2.one * 0.5f, 
            TextureXR.slices, dimension: TextureXR.dimension,
            name: "BlurBuffer"
        );
    }

    protected override void Execute(CustomPassContext ctx)
    {
        base.Execute(ctx);
        
        // Blur normals
        var resRadius = blurRadius * ctx.customColorBuffer.Value.rtHandleProperties.rtHandleScale.x;
        CustomPassUtils.GaussianBlur(
            ctx, 
            ctx.customColorBuffer.Value, 
            ctx.customColorBuffer.Value, 
            TempBuffer, 
            sampleCount,
            resRadius, 
            downSample: downsampleBuffer);
        
        // Calculate mesh position
        var camera = ctx.hdCamera.camera;
        var cameraTransform = camera.transform;
        var matrix = Matrix4x4.TRS(
            cameraTransform.position + cameraTransform.forward * (camera.nearClipPlane + 0.1f),
            cameraTransform.rotation,
            Vector3.one * 2f);

        // Render mesh
        CoreUtils.SetRenderTarget(ctx.cmd, ctx.cameraColorBuffer, ctx.cameraDepthBuffer);
        var pass = drawMaterial.FindPass("Forward");
        if (pass == -1)
        {
            pass = drawMaterial.FindPass("ForwardOnly");
        }
        ctx.cmd.DrawMesh(drawMesh, matrix, drawMaterial, 0, pass);
    }

    protected override void Cleanup()
    {
        base.Cleanup();
        
        TempBuffer.Release();
        TempBuffer = null;
    }
}
