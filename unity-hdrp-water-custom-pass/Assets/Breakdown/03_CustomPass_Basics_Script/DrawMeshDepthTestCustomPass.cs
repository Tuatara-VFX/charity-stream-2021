using UnityEngine;
using UnityEngine.Rendering;
using UnityEngine.Rendering.HighDefinition;

public class DrawMeshDepthTestCustomPass : CustomPass
{
    public Material drawMaterial;
    public Mesh drawMesh;
    public Transform drawTarget;
    
    protected override void Setup(ScriptableRenderContext renderContext, CommandBuffer cmd)
    {
        base.Setup(renderContext, cmd);
    }

    protected override void Execute(CustomPassContext ctx)
    {
        base.Execute(ctx);

        CoreUtils.SetRenderTarget(ctx.cmd, ctx.cameraColorBuffer, ctx.cameraDepthBuffer);
        var pass = drawMaterial.FindPass("DepthForwardOnly");
        ctx.cmd.DrawMesh(drawMesh, drawTarget.localToWorldMatrix, drawMaterial, 0, pass);
            
        pass = drawMaterial.FindPass("Forward");
        if (pass == -1)
        {
            pass = drawMaterial.FindPass("ForwardOnly");
        }
        ctx.cmd.DrawMesh(drawMesh, drawTarget.localToWorldMatrix, drawMaterial, 0, pass);
        
    }

    protected override void Cleanup()
    {
        base.Cleanup();
    }
}
