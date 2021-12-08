using UnityEngine;
using UnityEngine.Rendering;
using UnityEngine.Rendering.HighDefinition;

public class DrawMeshFullscreenCustomPass : CustomPass
{
    public Material drawMaterial;
    public Mesh drawMesh;
    
    protected override void Setup(ScriptableRenderContext renderContext, CommandBuffer cmd)
    {
        base.Setup(renderContext, cmd);
    }

    protected override void Execute(CustomPassContext ctx)
    {
        base.Execute(ctx);
        
         // Calculate mesh position
        var camera = ctx.hdCamera.camera;
        var cameraTransform = camera.transform;
        var matrix = Matrix4x4.TRS(
            cameraTransform.position + cameraTransform.forward * (camera.nearClipPlane + 0.1f),
            cameraTransform.rotation,
            Vector3.one * 2f);

        CoreUtils.SetRenderTarget(ctx.cmd, ctx.cameraColorBuffer /* ctx.cameraDepthBuffer */);
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
    }
}
