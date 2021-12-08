using SDFr;
using UnityEngine;
using UnityEngine.Rendering;

[ExecuteInEditMode]
public class RaymarchExample : MonoBehaviour
{
    public Shader shader;
    public Visualisation visualisation = Visualisation.Normal;

    public SDFData volumeA;
    public Transform volumeATransform;
    
    public SDFData volumeB;
    public Transform volumeBTransform;

    public float sphereRadius = 2f;
    public Transform sphere;

    public float boxSize = 2f;
    public Transform box;

    private CommandBuffer _cmd;
    private Material _material;
    private VolumeData[] _volumesData;
    private ComputeBuffer _volumes;
    
    private const int VolumeDataStride = 76;
    private struct VolumeData
    {
        public Matrix4x4 WorldToLocal;
        public Vector3 Extents;
    }
    
    bool CheckResources()
    {
        return shader != null && volumeATransform != null && volumeBTransform != null && sphere != null && box != null;
    }
    
    private void OnEnable()
    {
        _cmd = new CommandBuffer();
        _material = new Material(shader);
        _material.hideFlags = HideFlags.DontSave;
        _volumesData = new VolumeData[4];
        _volumes = new ComputeBuffer(4,VolumeDataStride);
        _volumes.SetData(_volumesData);
		OnSetKeywords();
    }

    private void OnDisable()
    {
        _cmd?.Dispose();
        if (_material != null)
        {
            DestroyImmediate(_material);
            _material = null;
        }
        _volumes?.Dispose();
    }

	void OnSetKeywords()
	{
		Shader.DisableKeyword("SDFr_VISUALIZE_STEPS");
		Shader.DisableKeyword("SDFr_VISUALIZE_HEATMAP");
		Shader.DisableKeyword("SDFr_VISUALIZE_DIST");

		switch( visualisation )
		{
			case Visualisation.IntensitySteps:  Shader.EnableKeyword("SDFr_VISUALIZE_STEPS"); break;
			case Visualisation.HeatmapSteps:	Shader.EnableKeyword("SDFr_VISUALIZE_HEATMAP"); break;
			case Visualisation.Distance:		Shader.EnableKeyword("SDFr_VISUALIZE_DIST"); break;
		}
	}

	// Editor call Only
	private void OnValidate()
	{
		OnSetKeywords();
	}

	void OnPostRender()
    {
        if (!CheckResources()) return;
        Camera cam = Camera.main;
        if (cam == null) return;

        //NOTE kind of overkill for just 2 volumes... but keeps it together
        _volumesData[0].WorldToLocal = volumeATransform.worldToLocalMatrix;
        _volumesData[0].Extents = volumeA.bounds.extents;
        
        _volumesData[1].WorldToLocal = volumeBTransform.worldToLocalMatrix;
        _volumesData[1].Extents = volumeB.bounds.extents;

		// Note: Assuming sphere and box are children!
		// If using single game object then scale is applied to bounds and localmatrix meaning its applied twice in shader!
		// Sphere
		_volumesData[2].WorldToLocal = sphere.worldToLocalMatrix;
        _volumesData[2].Extents = sphere.GetChild(0).GetComponent<MeshRenderer>().bounds.extents;
        // Box
        _volumesData[3].WorldToLocal = box.worldToLocalMatrix;
        _volumesData[3].Extents = box.GetChild(0).GetComponent<MeshRenderer>().bounds.extents;

        _volumes.SetData(_volumesData);
        
 
        AVolumeUtils.SetupRaymarchingMatrix(cam.fieldOfView,cam.worldToCameraMatrix,new Vector2(cam.pixelWidth, cam.pixelHeight));
        
        _cmd.Clear();
        
        _cmd.SetGlobalVector("_BlitScaleBiasRt",new Vector4(1f,1f,0f,0f));
        _cmd.SetGlobalVector("_BlitScaleBias", new Vector4(1f, 1f, 0f, 0f));
        _cmd.SetGlobalBuffer("_VolumeBuffer", _volumes);
        
        _cmd.SetGlobalTexture("_VolumeATex",volumeA.sdfTexture);
        _cmd.SetGlobalTexture("_VolumeBTex",volumeB.sdfTexture);
        
        Vector4 sphereData = sphere.transform.position;
        sphereData.w = sphereRadius;
        _cmd.SetGlobalVector("_Sphere",sphereData);
        
        Vector4 boxData = box.transform.position;
        boxData.w = boxSize;
        _cmd.SetGlobalVector("_Box",boxData);
        
        _cmd.DrawProcedural(Matrix4x4.identity, _material, 0, MeshTopology.Quads, 4, 1);
        Graphics.ExecuteCommandBuffer(_cmd);        
    }
    

}
